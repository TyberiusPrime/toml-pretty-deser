use crate::collector::TomlCollector;
use crate::traits::Visitor;
use crate::value::TomlValue;
use std::cell::RefCell;
use std::fmt::Write;
use std::ops::Range;
use std::rc::Rc;
use toml_edit::TomlError;

/// The failure states of deserialization
///
/// Use `DeserError.pretty(source_name)` to get a nice error message with code snippets and hints.
#[derive(Debug)]
pub enum DeserError<P>
where
    P: Visitor,
{
    /// TOML parsing failed. No `PartialT` available.
    /// A wrapper around [`toml_edit::TomlError`]
    ParsingFailure(TomlError, String),
    /// Parsing succeeded, but deserialization failed. `PartialT` available with whatever could be
    /// understood.
    /// Stores the source TOML and the partial result.
    DeserFailure(String, Box<TomlValue<P>>),
}

impl<P> DeserError<P>
where
    P: Visitor,
{
    #[must_use]
    pub fn pretty(&self, toml_filename: &str) -> String {
        let mut out = String::new();
        match self {
            Self::ParsingFailure(toml_error, source) => {
                let spans = vec![SpannedMessage {
                    span: toml_error.span().unwrap_or(0..0),
                    msg: toml_error.message().to_string(),
                }];
                out.push_str(&pretty_error_message(
                    toml_filename,
                    source,
                    &spans,
                    Some(&"See the TOML Spec: https://toml.io/en/v1.1.0".to_string()),
                ));
            }
            Self::DeserFailure(_source, _) => {
                let items = self.get_errors();
                let total = items.len();
                for (ii, item) in items.iter().enumerate() {
                    let ii = ii + 1;
                    let _ = writeln!(out, "Error {ii}/{total}");
                    out.push_str(&item.pretty(toml_filename));
                    out.push('\n');
                }
            }
        }
        out
    }

    #[must_use]
    pub fn get_errors(&self) -> Vec<HydratedAnnotatedError> {
        match self {
            Self::ParsingFailure(_, _) => vec![],
            Self::DeserFailure(source, partial) => {
                let collector = TomlCollector {
                    errors: Rc::new(RefCell::new(vec![])),
                    context_spans: Rc::new(RefCell::new(vec![])),
                    context_help: Rc::new(RefCell::new(vec![])),
                };
                partial.register_error(&collector);
                collector
                    .errors
                    .borrow_mut()
                    .drain(..)
                    .map(|error| HydratedAnnotatedError {
                        source: source.clone(),
                        inner: error,
                    })
                    .collect()
            }
        }
    }

    #[must_use]
    pub fn partial(&self) -> Option<&TomlValue<P>> {
        match self {
            Self::ParsingFailure(_, _) => None,
            Self::DeserFailure(_, partial) => Some(partial),
        }
    }
}

impl<P> std::fmt::Display for DeserError<P>
where
    P: Visitor + std::fmt::Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ParsingFailure(e, _) => write!(f, "TOML parse error: {e}"),
            Self::DeserFailure(_, _) => {
                let errors = self.get_errors();
                write!(
                    f,
                    "TOML deserialization failed with {} error(s) \
                     (call `.pretty(filename)` for a detailed report)",
                    errors.len()
                )
            }
        }
    }
}

impl<P> std::error::Error for DeserError<P>
where
    P: Visitor + std::fmt::Debug,
{
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Self::ParsingFailure(e, _) => Some(e),
            Self::DeserFailure(_, _) => None,
        }
    }
}

/// The non-pretty representation of deser errors.
#[derive(Debug, Clone)]
pub struct AnnotatedError {
    // spans on the TOML source and the messages we should display for them.
    pub spans: Vec<SpannedMessage>,
    /// A helpful hint that's shown after the TOML-snippet
    pub help: Option<String>,
}

/// Wrap a span and an error message together.
#[derive(Debug, Clone)]
pub struct SpannedMessage {
    pub span: Range<usize>,
    pub msg: String,
}

/// The user (developer) facing representation of issues with your TOML deserialization
///
/// Combines an [`AnnotatedError`] with the source TOML string to be able to produce pretty error
/// messages with code snippets and hints.
pub struct HydratedAnnotatedError {
    pub(crate) source: String,
    pub(crate) inner: AnnotatedError,
}

impl std::fmt::Debug for HydratedAnnotatedError {
    #[mutants::skip]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "HydratedAnnotatedError {{ ")?;
        write!(f, "{}", self.pretty("debug.toml"))?;
        writeln!(f, " }}")?;
        Ok(())
    }
}

impl AnnotatedError {
    #[must_use]
    pub fn unplaced(help: &str) -> Self {
        Self {
            spans: vec![],
            help: Some(help.to_string()),
        }
    }

    #[must_use]
    pub fn placed(span: Range<usize>, msg: &str, help: &str) -> Self {
        Self {
            spans: vec![SpannedMessage {
                span,
                msg: msg.to_string(),
            }],
            help: Some(help.to_string()),
        }
    }

    pub(crate) fn add_span(&mut self, span: Range<usize>, msg: &str) {
        self.spans.push(SpannedMessage {
            span,
            msg: msg.to_string(),
        });
    }
}

impl HydratedAnnotatedError {
    #[must_use]
    pub fn pretty(&self, source_name: &str) -> String {
        pretty_error_message(
            source_name,
            self.source.as_str(),
            &self.inner.spans,
            self.inner.help.as_ref(),
        )
    }
}

/// Detects a code line like "  9 │ action = 'Report'" and returns the source line number.
fn try_parse_code_line_no(line: &str) -> Option<usize> {
    let stripped = line.trim_start_matches(' ');
    if let Some(pos) = stripped.find(" \u{2502}") {
        let num_str = &stripped[..pos];
        if !num_str.is_empty() && num_str.chars().all(|c| c.is_ascii_digit()) {
            return num_str.parse().ok();
        }
    }
    None
}

/// Splits blockf into segments: each segment starts at a code line and includes
/// all following annotation lines (up to the next code line).
fn parse_block_segments(blockf: &str) -> Vec<(usize, String)> {
    let mut segments: Vec<(usize, String)> = Vec::new();
    let mut current: Option<(usize, String)> = None;
    for line in blockf.lines() {
        if let Some(line_no) = try_parse_code_line_no(line) {
            if let Some(prev) = current.take() {
                segments.push(prev);
            }
            current = Some((line_no, format!("{line}\n")));
        } else if let Some((_, ref mut content)) = current {
            content.push_str(line);
            content.push('\n');
        }
    }
    if let Some(last) = current {
        segments.push(last);
    }
    segments
}

/// Returns the 1-indexed line number of the nearest `[section]` header at or
/// before `code_line_no`. Returns 1 if none found.
fn find_section_start(source_lines: &[&str], code_line_no: usize) -> usize {
    let upper = code_line_no.min(source_lines.len());
    for i in (0..upper).rev() {
        if source_lines[i].trim_ascii_start().starts_with('[') {
            return i + 1;
        }
    }
    1
}

pub(crate) fn pretty_error_message(
    source_name: &str,
    source: &str,
    spans: &[SpannedMessage],
    help: Option<&String>,
) -> String {
    use codesnake::{Block, CodeWidth, Label, LineIndex};
    use std::fmt::Write;

    if spans.is_empty() {
        format!(
            "ConfigError at unknown location. Help text: {}",
            help.as_ref().map_or("None available", |x| x.as_str())
        )
    } else {
        let idx = LineIndex::new(source);
        let mut spans = spans.to_vec();
        spans.sort_by_key(|span| span.span.start);

        let mut labels = Vec::new();

        for span in spans.clone() {
            if span.msg.is_empty() {
                labels.push(Label::new(span.span));
            } else {
                labels.push(Label::new(span.span).with_text(span.msg));
            }
        }

        let block = Block::new(&idx, labels).unwrap_or_else(|| {
                let mut spans = spans.clone();
                spans.sort_by_key(|span| span.span.start);
                let span_str: Vec<_> = spans
                    .iter()
                    .map(|span| format!("{}..{}: {}", span.span.start, span.span.end, span.msg))
                    .collect();
                let span_str = span_str.join("\n");
                let final_message = format!(
                    "Error spans were overlapping so we were unable to process a pretty code block. Spans & messages:\n{span_str}"
                );
                let labels = vec![Label::new(0..0).with_text(final_message)];
                Block::new(&idx, labels).expect("can not fail")
            });
        let block = block.map_code(|c| CodeWidth::new(c, c.len()));
        let blockf: String = format!("{block}")
            .lines()
            .skip(1)
            .map(str::trim_end)
            .fold(String::new(), |acc, line| acc + line + "\n");
        let digits_needed = blockf.chars().position(|c| c == '│').map_or(1, |x| x - 1);

        let source_lines: Vec<&str> = source.lines().collect();
        let segments = parse_block_segments(&blockf);

        let mut out = String::new();
        writeln!(&mut out, "{}{}", block.prologue(), source_name).expect("can't fail");

        // last_shown_line: highest 1-indexed source line already emitted (0 = none yet).
        // Covers both codesnake code lines and context lines we added.
        let mut last_shown_line: usize = 0;

        for (code_line_no, segment_text) in &segments {
            let code_line_no = *code_line_no;

            let section_start = find_section_start(&source_lines, code_line_no);

            // Start context from the line after what we last showed, but never
            // before the nearest section header.
            let context_start = if last_shown_line == 0 {
                section_start
            } else {
                (last_shown_line + 1).max(section_start)
            };
            let context_end = code_line_no.saturating_sub(1);

            if context_start <= context_end {
                writeln!(&mut out, "{:digits_needed$} ┆", "").expect("can't fail");
                for ln in context_start..=context_end {
                    if ln >= 1 && ln <= source_lines.len() {
                        writeln!(
                            &mut out,
                            "{:>digits_needed$} │ {}",
                            ln,
                            source_lines[ln - 1]
                        )
                        .expect("can't fail");
                    }
                }
            }

            write!(&mut out, "{segment_text}").expect("can't fail");
            last_shown_line = code_line_no;
        }

        writeln!(&mut out, "{}", block.epilogue()).expect("can't fail");

        if let Some(help) = help.as_ref()
            && !help.is_empty()
        {
            let mut first = true;
            write!(&mut out, "Hint: ").expect("Can't fail");
            for line in help.lines() {
                if !first {
                    write!(&mut out, "      ").expect("can't fail");
                }
                first = false;
                writeln!(&mut out, "{line}").expect("can't fail");
            }
        }
        out
    }
}
