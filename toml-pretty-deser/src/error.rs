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

#[allow(clippy::too_many_lines)]
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

        let previous_newline = source[..spans[0].span.start].rfind('\n');
        let this_line_is_block_start = source[previous_newline.unwrap_or(0)..]
            .trim_ascii_start()
            .starts_with('[');

        let mut labels = Vec::new();

        for span in spans.clone() {
            labels.push(Label::new(span.span).with_text(span.msg));
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

        let lines_before = match previous_newline {
            None => String::new(),
            Some(previous_newline) => {
                let lines: Vec<&str> = {
                    let upto_span = &source[..previous_newline];
                    upto_span.lines().collect()
                };
                let mut seen_opening = false;
                let mut lines_before: Vec<_> = {
                    if this_line_is_block_start {
                        vec![]
                    } else {
                        lines
                            .into_iter()
                            .enumerate()
                            .rev()
                            .take_while(move |x| {
                                if x.1.trim_ascii_start().starts_with('[') {
                                    seen_opening = true;
                                    true
                                } else {
                                    !seen_opening
                                }
                            })
                            .map(|(line_no, line)| {
                                format!("{:>digits_needed$} │ {line}", line_no + 1,)
                            })
                            .collect()
                    }
                };
                lines_before.reverse();
                lines_before.join("\n")
            }
        };

        let mut out = String::new();
        writeln!(&mut out, "{}{}", block.prologue(), source_name).expect("can't fail");
        write!(&mut out, "{:digits_needed$} ┆\n{}", " ", lines_before).expect("can't fail");
        if !lines_before.is_empty() {
            writeln!(&mut out).expect("can't fail");
        }

        write!(&mut out, "{blockf}").expect("can't fail");
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
