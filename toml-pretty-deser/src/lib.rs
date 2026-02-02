use std::collections::HashSet;
use std::fmt::Display;
use std::{cell::RefCell, ops::Range, rc::Rc};
use toml_edit::{Document, TomlError};

pub use toml_pretty_deser_macros::{StringNamedEnum, make_partial};

pub trait StringNamedEnum: Sized + Clone {
    fn all_variant_names() -> &'static [&'static str];
    fn from_str(s: &str) -> Option<Self>;
}

/// Controls how field names are matched against TOML keys
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum FieldMatchMode {
    /// Exact match only (field name must match exactly)
    #[default]
    Exact,
    /// Case insensitive match (case insensitive exact match only)
    UpperLower,
    /// Any case variant match (allows camelCase, snake_case, kebab-case, etc.)
    AnyCase,
}

impl FieldMatchMode {
    /// Normalize a name according to the match mode
    pub fn normalize(&self, name: &str) -> String {
        match self {
            FieldMatchMode::Exact => name.to_string(),
            FieldMatchMode::UpperLower => name.to_lowercase(),
            FieldMatchMode::AnyCase => normalize_to_no_case(name),
        }
    }

    /// Check if two names match under this mode
    pub fn matches(&self, a: &str, b: &str) -> bool {
        self.normalize(a) == self.normalize(b)
    }
}

/// Convert any case variant to snake_case for comparison
/// Supports: camelCase, UpperCamelCase, snake_case, kebab-case, SHOUTY_SNAKE_CASE, Train-Case
pub fn normalize_to_no_case(s: &str) -> String {
    s.chars()
        .filter_map(|c| match c {
            'A'..='Z' => Some(c.to_lowercase().next().unwrap()),
            'a'..='z' | '0'..='9' => Some(c),
            _ => None,
        })
        .collect()
    // let mut result = String::with_capacity(s.len() * 2);
    // let mut chars = s.chars().peekable();
    // let mut prev_was_uppercase = false;
    // let mut prev_was_separator = true;
    //
    // while let Some(c) = chars.next() {
    //     if c == '-' || c == '_' {
    //         if !prev_was_separator {
    //             result.push('_');
    //         }
    //         prev_was_separator = true;
    //         prev_was_uppercase = false;
    //     } else if c.is_uppercase() {
    //         // Check if we need to insert an underscore
    //         let next_is_lowercase = chars
    //             .peek()
    //             .map(|next| next.is_lowercase())
    //             .unwrap_or(false);
    //
    //         if !prev_was_separator {
    //             // Insert underscore if:
    //             // 1. Previous was lowercase (camelCase to snake_case)
    //             // 2. Previous was uppercase AND next is lowercase (HTMLParser to html_parser)
    //             if !prev_was_uppercase || next_is_lowercase {
    //                 result.push('_');
    //             }
    //         }
    //
    //         result.push(c.to_lowercase().next().unwrap());
    //         prev_was_uppercase = true;
    //         prev_was_separator = false;
    //     } else {
    //         // Lowercase letter or other char
    //         result.push(c);
    //         prev_was_uppercase = false;
    //         prev_was_separator = false;
    //     }
    // }
    //
    // // Remove leading/trailing underscores
    // while result.starts_with('_') {
    //     result.remove(0);
    // }
    // while result.ends_with('_') {
    //     result.pop();
    // }
    //
    // result
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_normalize_no_case() {
        assert_eq!(normalize_to_no_case("snake_case"), "snakecase");
        assert_eq!(normalize_to_no_case("camelCase"), "camelcase");
        assert_eq!(
            normalize_to_no_case("UpperCamelCase"),
            "uppercamelcase"
        );
        assert_eq!(normalize_to_no_case("kebab-case"), "kebabcase");
        assert_eq!(
            normalize_to_no_case("SHOUTY_SNAKE_CASE"),
            "shoutysnakecase"
        );
        assert_eq!(normalize_to_no_case("Train-Case"), "traincase");
        assert_eq!(
            normalize_to_no_case("mixed_Case-Variant"),
            "mixedcasevariant"
        );
        assert_eq!(normalize_to_no_case("already_snake"), "alreadysnake");
        assert_eq!(normalize_to_no_case("HTMLParser"), "htmlparser");
        assert_eq!(
            normalize_to_no_case("getHTTPResponse"),
            "gethttpresponse"
        );
    }

    #[test]
    fn test_field_match_mode() {
        let exact = FieldMatchMode::Exact;
        assert!(exact.matches("field_name", "field_name"));
        assert!(!exact.matches("field_name", "Field_Name"));
        assert!(!exact.matches("field_name", "fieldName"));

        let upper_lower = FieldMatchMode::UpperLower;
        assert!(upper_lower.matches("field_name", "field_name"));
        assert!(upper_lower.matches("field_name", "FIELD_NAME"));
        assert!(upper_lower.matches("field_name", "Field_Name"));
        assert!(!upper_lower.matches("field_name", "fieldName"));

        let any_case = FieldMatchMode::AnyCase;
        assert!(any_case.matches("field_name", "field_name"));
        assert!(any_case.matches("field_name", "fieldName"));
        assert!(any_case.matches("field_name", "FieldName"));
        assert!(any_case.matches("field_name", "field-name"));
        assert!(any_case.matches("field_name", "FIELD_NAME"));
        assert!(any_case.matches("field_name", "Field-Name"));
    }
}

pub trait AsEnum<E>: Sized {
    fn as_enum(self) -> TomlValue<E>;
}

impl<E: StringNamedEnum> AsEnum<E> for TomlValue<String> {
    fn as_enum(self) -> TomlValue<E> {
        match &self.state {
            TomlValueState::Ok { span } => {
                if let Some(ref s) = self.value {
                    match E::from_str(s) {
                        Some(enum_val) => TomlValue {
                            value: Some(enum_val),
                            required: self.required,
                            state: TomlValueState::Ok { span: span.clone() },
                        },
                        None => {
                            let valid_names = E::all_variant_names().join(", ");
                            TomlValue {
                                value: None,
                                required: self.required,
                                state: TomlValueState::ValidationFailed {
                                    span: span.clone(),
                                    message: format!(
                                        "Invalid enum variant '{}'. Valid variants are: {}",
                                        s, valid_names
                                    ),
                                },
                            }
                        }
                    }
                } else {
                    TomlValue {
                        value: None,
                        required: self.required,
                        state: TomlValueState::ValidationFailed {
                            span: span.clone(),
                            message: "Cannot convert empty value to enum".to_string(),
                        },
                    }
                }
            }
            TomlValueState::Missing { key, parent_span } => TomlValue {
                value: None,
                required: self.required,
                state: TomlValueState::Missing {
                    key: key.clone(),
                    parent_span: parent_span.clone(),
                },
            },
            _ => TomlValue {
                value: None,
                required: self.required,
                state: self.state.clone(),
            },
        }
    }
}

impl<E: StringNamedEnum> AsEnum<Option<E>> for TomlValue<Option<String>> {
    fn as_enum(self) -> TomlValue<Option<E>> {
        match &self.state {
            TomlValueState::Ok { span } => {
                if let Some(ref opt_s) = self.value {
                    if let Some(s) = opt_s {
                        match E::from_str(&s) {
                            Some(enum_val) => TomlValue {
                                value: Some(Some(enum_val)),
                                required: self.required,
                                state: TomlValueState::Ok { span: span.clone() },
                            },
                            None => {
                                let valid_names = E::all_variant_names().join(", ");
                                TomlValue {
                                    value: None,
                                    required: self.required,
                                    state: TomlValueState::ValidationFailed {
                                        span: span.clone(),
                                        message: format!(
                                            "Invalid enum variant '{}'. Valid variants are: {}",
                                            s, valid_names
                                        ),
                                    },
                                }
                            }
                        }
                    } else {
                        // None value is valid for Option<E>
                        TomlValue {
                            value: Some(None),
                            required: self.required,
                            state: TomlValueState::Ok { span: span.clone() },
                        }
                    }
                } else {
                    TomlValue {
                        value: None,
                        required: self.required,
                        state: TomlValueState::ValidationFailed {
                            span: span.clone(),
                            message: "Cannot convert empty value to enum".to_string(),
                        },
                    }
                }
            }
            TomlValueState::Missing {
                key: _,
                parent_span,
            } => TomlValue {
                value: Some(None),
                required: self.required,
                state: TomlValueState::Ok {
                    span: parent_span.clone(),
                },
            },
            _ => TomlValue {
                value: None,
                required: self.required,
                state: self.state.clone(),
            },
        }
    }
}

impl<E: StringNamedEnum> AsEnum<Vec<E>> for TomlValue<Vec<String>> {
    fn as_enum(self) -> TomlValue<Vec<E>> {
        match &self.state {
            TomlValueState::Ok { span } => {
                if let Some(ref strings) = self.value {
                    let mut values = Vec::with_capacity(strings.len());
                    let mut has_error = false;
                    let mut error_message = String::new();

                    for s in strings {
                        match E::from_str(s) {
                            Some(enum_val) => values.push(enum_val),
                            None => {
                                has_error = true;
                                let valid_names = E::all_variant_names().join(", ");
                                error_message = format!(
                                    "Invalid enum variant '{}'. Valid variants are: {}",
                                    s, valid_names
                                );
                                break;
                            }
                        }
                    }

                    if has_error {
                        TomlValue {
                            value: None,
                            required: self.required,
                            state: TomlValueState::ValidationFailed {
                                span: span.clone(),
                                message: error_message,
                            },
                        }
                    } else {
                        TomlValue {
                            value: Some(values),
                            required: self.required,
                            state: TomlValueState::Ok { span: span.clone() },
                        }
                    }
                } else {
                    TomlValue {
                        value: None,
                        required: self.required,
                        state: TomlValueState::ValidationFailed {
                            span: span.clone(),
                            message: "Cannot convert empty value to enum".to_string(),
                        },
                    }
                }
            }
            TomlValueState::Missing { key, parent_span } => TomlValue {
                value: None,
                required: self.required,
                state: TomlValueState::Missing {
                    key: key.clone(),
                    parent_span: parent_span.clone(),
                },
            },
            _ => TomlValue {
                value: None,
                required: self.required,
                state: self.state.clone(),
            },
        }
    }
}

impl<E: StringNamedEnum> AsEnum<Option<Vec<E>>> for TomlValue<Option<Vec<String>>> {
    fn as_enum(self) -> TomlValue<Option<Vec<E>>> {
        match &self.state {
            TomlValueState::Ok { span } => {
                if let Some(ref opt_strings) = self.value {
                    if let Some(strings) = opt_strings {
                        let mut values = Vec::with_capacity(strings.len());
                        let mut has_error = false;
                        let mut error_message = String::new();

                        for s in strings {
                            match E::from_str(&s) {
                                Some(enum_val) => values.push(enum_val),
                                None => {
                                    has_error = true;
                                    let valid_names = E::all_variant_names().join(", ");
                                    error_message = format!(
                                        "Invalid enum variant '{}'. Valid variants are: {}",
                                        s, valid_names
                                    );
                                    break;
                                }
                            }
                        }

                        if has_error {
                            TomlValue {
                                value: None,
                                required: self.required,
                                state: TomlValueState::ValidationFailed {
                                    span: span.clone(),
                                    message: error_message,
                                },
                            }
                        } else {
                            TomlValue {
                                value: Some(Some(values)),
                                required: self.required,
                                state: TomlValueState::Ok { span: span.clone() },
                            }
                        }
                    } else {
                        // None value is valid for Option<Vec<E>>
                        TomlValue {
                            value: Some(None),
                            required: self.required,
                            state: TomlValueState::Ok { span: span.clone() },
                        }
                    }
                } else {
                    TomlValue {
                        value: None,
                        required: self.required,
                        state: TomlValueState::ValidationFailed {
                            span: span.clone(),
                            message: "Cannot convert empty value to enum".to_string(),
                        },
                    }
                }
            }
            TomlValueState::Missing {
                key: _,
                parent_span,
            } => TomlValue {
                value: Some(None),
                required: self.required,
                state: TomlValueState::Ok {
                    span: parent_span.clone(),
                },
            },
            _ => TomlValue {
                value: None,
                required: self.required,
                state: self.state.clone(),
            },
        }
    }
}

pub fn deserialize<P, T>(source: &str) -> Result<T, DeserError<P>>
where
    P: FromTomlTable<()> + VerifyFromToml<()> + ToConcrete<T>,
{
    deserialize_with_mode(source, FieldMatchMode::default())
}

pub fn deserialize_with_mode<P, T>(source: &str, mode: FieldMatchMode) -> Result<T, DeserError<P>>
where
    P: FromTomlTable<()> + VerifyFromToml<()> + ToConcrete<T>,
{
    let parsed_toml = source.parse::<Document<String>>()?;
    let source = Rc::new(RefCell::new(source.to_string()));

    let errors = Rc::new(RefCell::new(Vec::new()));
    let mut helper = TomlHelper::new(parsed_toml.as_table(), errors.clone(), mode);

    let partial = P::from_toml_table(&mut helper, &()).verify(&mut helper, &());
    helper.deny_unknown();

    partial.collect_errors(&errors);
    if !errors.borrow().is_empty() {
        return Err(DeserError::DeserFailure(
            helper.into_inner(&source),
            partial,
        ));
    }

    if partial.can_concrete() {
        Ok(partial
            .to_concrete()
            .expect("can_concrete() returned true; qed"))
    } else {
        Err(DeserError::StillIncomplete(
            helper.into_inner(&source),
            partial,
        ))
    }
}

fn suggest_alternatives<T: AsRef<str>>(current: &str, available: &[T]) -> String {
    if current.is_empty() {
        let mut sorted: Vec<&str> = available.iter().map(AsRef::as_ref).collect::<Vec<&str>>();
        sorted.sort_unstable();
        return format!("Available are: {}", format_quoted_list(&sorted));
    }

    let mut distances: Vec<(usize, &str)> = available
        .iter()
        .map(|item| {
            let item_str = item.as_ref();
            let dist = strsim::levenshtein(current, item_str);
            (dist, item_str)
        })
        .collect();

    distances.sort_by_key(|k| k.0);

    let closest: Vec<&str> = distances.into_iter().take(3).map(|(_, s)| s).collect();

    if closest.is_empty() {
        "All known keys have been used.".to_string()
    } else {
        format!("Did you mean: {}?", format_quoted_list(&closest))
    }
}

fn format_quoted_list(items: &[&str]) -> String {
    match items {
        [] => String::new(),
        [single] => format!("'{}'", single),
        [first, second] => format!("'{}' or '{}'", first, second),
        rest => {
            let (last, init) = rest.split_last().expect("can't fail");
            let start = init
                .iter()
                .map(|s| format!("'{}'", s))
                .collect::<Vec<_>>()
                .join(", ");
            format!("{}, or '{}'", start, last)
        }
    }
}

#[derive(Debug)]
pub enum DeserError<P> {
    ParsingFailure(TomlError),
    DeserFailure(Vec<HydratedAnnotatedError>, P),
    StillIncomplete(Vec<HydratedAnnotatedError>, P),
}

impl<P> Display for DeserError<P> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DeserError::ParsingFailure(e) => write!(f, "TOML parsing error: {}", e),
            DeserError::DeserFailure(errors, _) => {
                write!(f, "Deserialization failed with {} errors", errors.len())
            }
            DeserError::StillIncomplete(errors, _) => {
                write!(f, "Incomplete deserialization with {} errors", errors.len())
            }
        }
    }
}

impl<P: std::fmt::Debug> std::error::Error for DeserError<P> {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            DeserError::ParsingFailure(e) => Some(e),
            _ => None,
        }
    }
}

impl<P> From<TomlError> for DeserError<P> {
    fn from(value: TomlError) -> Self {
        DeserError::ParsingFailure(value)
    }
}

pub trait FromTomlTable<T> {
    fn from_toml_table(helper: &mut TomlHelper<'_>, _partial: &T) -> Self
    where
        Self: Sized;
}

pub trait VerifyFromToml<T> {
    fn verify(self, _helper: &mut TomlHelper<'_>, _partial: &T) -> Self
    where
        Self: Sized,
    {
        self
    }
}

pub trait ToConcrete<T> {
    fn collect_errors(&self, errors: &Rc<RefCell<Vec<AnnotatedError>>>);
    fn can_concrete(&self) -> bool;
    fn to_concrete(self) -> Option<T>;
}

#[derive(Debug, Clone)]
pub struct AnnotatedError {
    pub spans: Vec<SpannedMessage>,
    pub help: Option<String>,
}

#[derive(Debug, Clone)]
pub struct SpannedMessage {
    pub span: Range<usize>,
    pub msg: String,
}

#[derive(Debug)]
pub struct HydratedAnnotatedError {
    pub source: Rc<RefCell<String>>,
    pub inner: AnnotatedError,
}

impl AnnotatedError {
    pub fn unplaced(help: &str) -> Self {
        AnnotatedError {
            spans: vec![],
            help: Some(help.to_string()),
        }
    }

    pub fn placed(span: Range<usize>, msg: &str, help: &str) -> Self {
        AnnotatedError {
            spans: vec![SpannedMessage {
                span,
                msg: msg.to_string(),
            }],
            help: Some(help.to_string()),
        }
    }
}

pub trait AnnotatedErrorExt {
    fn add_span(&mut self, span: Range<usize>, msg: &str);
}

impl AnnotatedErrorExt for AnnotatedError {
    fn add_span(&mut self, span: Range<usize>, msg: &str) {
        self.spans.push(SpannedMessage {
            span,
            msg: msg.to_string(),
        });
    }
}

impl HydratedAnnotatedError {
    pub fn pretty(&self, source_name: &str) -> String {
        use bstr::{BStr, ByteSlice};
        use codesnake::{Block, CodeWidth, Label, LineIndex};
        use std::fmt::Write;

        let source = self.source.borrow();

        if !self.inner.spans.is_empty() {
            let idx = LineIndex::new(&source);
            let mut spans = self.inner.spans.clone();
            spans.sort_by_key(|span| span.span.start);

            let mut previous_newline =
                memchr::memmem::rfind(&source.as_bytes()[..spans[0].span.start], b"\n");
            let this_line_is_block_start = source.as_bytes()[previous_newline.unwrap_or(0)..]
                .trim_ascii_start()
                .starts_with(b"[");
            if this_line_is_block_start {
                previous_newline = None;
            }

            let mut labels = Vec::new();

            for span in spans.into_iter() {
                labels.push(Label::new(span.span).with_text(span.msg));
            }

            let block = Block::new(&idx, labels).unwrap_or_else(|| {
                let mut spans = self.inner.spans.clone();
                spans.sort_by_key(|span| span.span.start);
                let span_str: Vec<_> = spans
                    .iter()
                    .map(|span| format!("{}..{}: {}", span.span.start, span.span.end, span.msg))
                    .collect();
                let span_str = span_str.join("\n");
                let final_message = format!(
                    "Error spans were overlapping so we were unable to process a pretty code block. Spans & messages:\n{}",
                    span_str
                );
                let labels = vec![Label::new(0..0).with_text(final_message)];
                Block::new(&idx, labels).expect("can not fail")
            });

            let (lines_before, digits_needed) = match previous_newline {
                None => ("".to_string(), 1),
                Some(previous_newline) => {
                    let upto_span = &BStr::new(source.as_bytes())[..previous_newline];
                    let lines: Vec<_> = upto_span.lines().collect();
                    let str_line_no = format!("{}", lines.len());
                    let digits_needed = str_line_no.len();
                    let mut seen_opening = false;
                    let mut lines_before: Vec<_> = lines
                        .into_iter()
                        .enumerate()
                        .map(|(line_no, line)| (line_no, line))
                        .rev()
                        .take_while(move |x| {
                            if BStr::new(x.1).trim_ascii_start().starts_with(b"[") {
                                seen_opening = true;
                                true
                            } else {
                                !seen_opening
                            }
                        })
                        .map(|(line_no, line)| {
                            format!(
                                "{:>digits_needed$} │ {}",
                                line_no + 1,
                                std::string::String::from_utf8_lossy(line)
                            )
                        })
                        .collect();
                    lines_before.reverse();
                    (lines_before.join("\n"), digits_needed)
                }
            };

            let block = block.map_code(|c| CodeWidth::new(c, c.len()));
            let mut out = String::new();
            writeln!(&mut out, "{}{}", block.prologue(), source_name).expect("can't fail");
            write!(&mut out, " {:digits_needed$}┆\n{}\n", " ", lines_before).expect("can't fail");
            let blockf: String = format!("{}", block)
                .lines()
                .skip(1)
                .map(|x| format!("{}\n", x))
                .collect();
            write!(&mut out, "{}", blockf).expect("can't fail");
            writeln!(&mut out, "{}", block.epilogue()).expect("can't fail");

            if let Some(help) = self.inner.help.as_ref() {
                if !help.is_empty() {
                    let mut first = true;
                    write!(&mut out, "Hint: ").expect("Can't fail");
                    for line in help.lines() {
                        if !first {
                            write!(&mut out, "      ").expect("can't fail");
                        }
                        first = false;
                        writeln!(&mut out, "{}", line).expect("can't fail");
                    }
                }
            }
            out
        } else {
            format!(
                "ConfigError at unknown location. Help text: {}",
                self.inner
                    .help
                    .as_ref()
                    .map(|x| x.as_str())
                    .unwrap_or("None available")
            )
        }
    }
}

/// Stores information about expected fields and their aliases
#[derive(Debug, Clone)]
pub struct FieldInfo {
    pub name: String,
    pub aliases: Vec<&'static str>,
}

impl FieldInfo {
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            aliases: Vec::new(),
        }
    }

    pub fn with_alias(mut self, alias: &'static str) -> Self {
        self.aliases.push(alias);
        self
    }

    pub fn with_aliases(mut self, aliases: &Vec<&'static str>) -> Self {
        self.aliases.extend(aliases);
        self
    }

    /// Get all normalized names for this field (primary name + aliases)
    pub fn all_normalized_names(&self, mode: &FieldMatchMode) -> Vec<String> {
        let mut names = vec![mode.normalize(&self.name)];
        for alias in &self.aliases {
            names.push(mode.normalize(alias));
        }
        names
    }
}

pub struct TomlHelper<'a> {
    table: Option<&'a toml_edit::Table>,
    inline_table: Option<&'a toml_edit::InlineTable>,
    /// Expected field info (what we allow to see)
    expected: Vec<FieldInfo>,
    /// Normalized names that were actually observed (matched against table keys)
    observed: Vec<String>,
    /// Original field names that were allowed
    allowed: Vec<String>,
    pub errors: Rc<RefCell<Vec<AnnotatedError>>>,
    pub match_mode: FieldMatchMode,
}

impl<'a> TomlHelper<'a> {
    pub fn new(
        table: &'a toml_edit::Table,
        errors: Rc<RefCell<Vec<AnnotatedError>>>,
        match_mode: FieldMatchMode,
    ) -> Self {
        Self {
            table: Some(table),
            inline_table: None,
            expected: vec![],
            observed: vec![],
            allowed: vec![],
            errors,
            match_mode,
        }
    }

    pub fn new_inline(
        inline_table: &'a toml_edit::InlineTable,
        errors: Rc<RefCell<Vec<AnnotatedError>>>,
        match_mode: FieldMatchMode,
    ) -> Self {
        Self {
            table: None,
            inline_table: Some(inline_table),
            expected: vec![],
            observed: vec![],
            allowed: vec![],
            errors,
            match_mode,
        }
    }

    pub fn with_match_mode(&mut self, mode: FieldMatchMode) {
        self.match_mode = mode;
    }

    pub fn into_inner(self, source: &Rc<RefCell<String>>) -> Vec<HydratedAnnotatedError> {
        self.errors
            .borrow_mut()
            .drain(..)
            .map(|error| HydratedAnnotatedError {
                source: source.clone(),
                inner: error,
            })
            .collect()
    }

    /// Register a field with optional aliases
    pub fn expect_field(&mut self, name: impl Into<String>, aliases: &Vec<&'static str>) {
        let field_info = FieldInfo::new(name).with_aliases(aliases);
        self.expected.push(field_info);
    }

    /// Find a key in the table that matches the given field name (considering aliases and match mode)
    fn find_matching_key(
        &self,
        name: &str,
        aliases: &[&'static str],
    ) -> Option<(String, Option<toml_edit::Item>)> {
        let _normalized_target = self.match_mode.normalize(name);
        let candidates = std::iter::once(name.to_string())
            .chain(aliases.iter().map(|x| x.to_string()))
            .collect::<Vec<_>>();

        // Collect all table keys
        let table_keys: Vec<String> = if let Some(table) = self.table {
            table.iter().map(|(k, _)| k.to_string()).collect()
        } else if let Some(inline_table) = self.inline_table {
            inline_table.iter().map(|(k, _)| k.to_string()).collect()
        } else {
            vec![]
        };

        // Try to find a match
        for candidate in &candidates {
            let _normalized_candidate = self.match_mode.normalize(candidate);

            // First try exact match (faster)
            if self.match_mode == FieldMatchMode::Exact {
                if let Some(table) = self.table {
                    if let Some(item) = table.get(candidate) {
                        return Some((candidate.clone(), Some(item.clone())));
                    }
                } else if let Some(inline_table) = self.inline_table {
                    if let Some(value) = inline_table.get(candidate) {
                        return Some((
                            candidate.clone(),
                            Some(toml_edit::Item::Value(value.clone())),
                        ));
                    }
                }
            } else {
                // For non-exact modes, compare normalized names
                for table_key in &table_keys {
                    if self.match_mode.matches(candidate, table_key) {
                        if let Some(table) = self.table {
                            if let Some(item) = table.get(table_key) {
                                return Some((table_key.clone(), Some(item.clone())));
                            }
                        } else if let Some(inline_table) = self.inline_table {
                            if let Some(value) = inline_table.get(table_key) {
                                return Some((
                                    table_key.clone(),
                                    Some(toml_edit::Item::Value(value.clone())),
                                ));
                            }
                        }
                    }
                }
            }
        }

        None
    }

    pub fn get<T>(&mut self, key: &str) -> TomlValue<T>
    where
        TomlValue<T>: FromTomlItem,
    {
        self.get_with_aliases(key, vec![])
    }

    pub fn get_with_aliases<T>(&mut self, key: &str, aliases: Vec<&'static str>) -> TomlValue<T>
    where
        TomlValue<T>: FromTomlItem,
    {
        let parent_span = if let Some(table) = self.table {
            table.span().unwrap_or(0..0)
        } else if let Some(inline_table) = self.inline_table {
            inline_table.span().unwrap_or(0..0)
        } else {
            0..0
        };

        // Register this field as expected
        self.expect_field(key, &aliases);

        // Try to find a matching key (considering aliases and match mode)
        match self.find_matching_key(key, &aliases) {
            Some((matched_key, Some(item))) => {
                let res: TomlValue<T> = FromTomlItem::from_toml_item(&item, parent_span);
                match &res.state {
                    TomlValueState::NotSet => unreachable!(),
                    TomlValueState::Missing { .. } => {}
                    _ => {
                        self.allowed.push(key.to_string());
                        self.observed.push(self.match_mode.normalize(&matched_key));
                    }
                }
                res
            }
            _ => {
                // No match found
                let key_str = key.to_string();
                let mut res: TomlValue<T> =
                    FromTomlItem::from_toml_item(&toml_edit::Item::None, parent_span);
                if let TomlValueState::Missing { ref mut key, .. } = res.state {
                    if key.is_empty() {
                        *key = key_str;
                    }
                }
                res
            }
        }
    }

    pub fn add_err(&self, err: AnnotatedError) {
        self.errors.borrow_mut().push(err);
    }

    pub fn add_err_by_key(&self, key: &str, msg: &str, help: &str) {
        let span = if let Some(table) = self.table {
            table.key(key).and_then(|item| item.span()).unwrap_or(0..0)
        } else if let Some(inline_table) = self.inline_table {
            // For inline tables, we can't easily get the key span, so use the table span
            inline_table.span().unwrap_or(0..0)
        } else {
            0..0
        };
        self.add_err_by_span(span.clone(), msg, help);
    }

    pub fn add_err_by_span(&self, span: Range<usize>, msg: &str, help: &str) {
        self.errors
            .borrow_mut()
            .push(AnnotatedError::placed(span.clone(), msg, help));
    }

    pub fn deny_unknown(&self) {
        // Build set of normalized expected names (including aliases)
        let mut expected_normalized: HashSet<String> = HashSet::new();
        for field_info in &self.expected {
            for normalized_name in field_info.all_normalized_names(&self.match_mode) {
                expected_normalized.insert(normalized_name);
            }
        }

        // Collect all keys from either table type
        let keys: Vec<String> = if let Some(table) = self.table {
            table.iter().map(|(k, _)| k.to_string()).collect()
        } else if let Some(inline_table) = self.inline_table {
            inline_table.iter().map(|(k, _)| k.to_string()).collect()
        } else {
            vec![]
        };

        // Build set of observed normalized names
        let observed_set: HashSet<String> = self.observed.iter().cloned().collect();

        for key in keys {
            let normalized_key = self.match_mode.normalize(&key);

            // Check if this key was observed (i.e., it matched an expected field)
            if !observed_set.contains(&normalized_key) {
                // This is an unknown key - find available (expected but not yet observed) fields
                let still_available: Vec<_> = expected_normalized
                    .iter()
                    .filter(|expected| !observed_set.contains(*expected))
                    .map(|s| s.as_str())
                    .collect();

                self.add_err_by_key(
                    &key,
                    &format!("Unknown key: {}", key),
                    &suggest_alternatives(&key, &still_available),
                );
            }
        }
    }
}

pub trait FromTomlItem {
    fn is_optional() -> bool;
    fn from_toml_item(item: &toml_edit::Item, parent_span: Range<usize>) -> Self;
}

macro_rules! impl_from_toml_item_integer {
    ($ty:ty, $name:expr) => {
        impl FromTomlItem for TomlValue<$ty> {
            fn is_optional() -> bool {
                true
            }

            fn from_toml_item(item: &toml_edit::Item, parent_span: Range<usize>) -> Self {
                match item {
                    toml_edit::Item::None => TomlValue {
                        value: None,
                        required: true,
                        state: TomlValueState::Missing {
                            key: "".to_string(),
                            parent_span,
                        },
                    },
                    toml_edit::Item::Value(toml_edit::Value::Integer(formatted)) => {
                        let value_i64 = *formatted.value();
                        if value_i64 < <$ty>::MIN as i64 || value_i64 > <$ty>::MAX as i64 {
                            TomlValue {
                                required: true,
                                value: None,
                                state: TomlValueState::ValidationFailed {
                                    span: formatted.span().unwrap_or(parent_span.clone()),
                                    message: format!(
                                        "integer out of range. Accepted: {}..{}",
                                        <$ty>::MIN,
                                        <$ty>::MAX
                                    ),
                                },
                            }
                        } else {
                            TomlValue {
                                required: true,
                                value: Some(value_i64 as $ty),
                                state: TomlValueState::Ok {
                                    span: formatted.span().unwrap_or(parent_span.clone()),
                                },
                            }
                        }
                    }
                    toml_edit::Item::Value(value) => TomlValue {
                        required: true,
                        value: None,
                        state: TomlValueState::WrongType {
                            span: value.span().unwrap_or(parent_span.clone()),
                            expected: $name,
                            found: value.type_name(),
                        },
                    },
                    toml_edit::Item::Table(value) => TomlValue {
                        required: true,
                        value: None,
                        state: TomlValueState::WrongType {
                            span: value.span().unwrap_or(parent_span.clone()),
                            expected: $name,
                            found: "table",
                        },
                    },
                    toml_edit::Item::ArrayOfTables(value) => TomlValue {
                        required: true,
                        value: None,
                        state: TomlValueState::WrongType {
                            span: value.span().unwrap_or(parent_span.clone()),
                            expected: $name,
                            found: "array of tables",
                        },
                    },
                }
            }
        }
    };
}

impl_from_toml_item_integer!(i8, "i8");
impl_from_toml_item_integer!(u8, "u8");
impl_from_toml_item_integer!(i16, "i16");
impl_from_toml_item_integer!(i32, "i32");
impl_from_toml_item_integer!(u32, "u32");
impl_from_toml_item_integer!(i64, "i64");
impl_from_toml_item_integer!(u64, "u64");

impl FromTomlItem for TomlValue<f64> {
    fn is_optional() -> bool {
        true
    }

    fn from_toml_item(item: &toml_edit::Item, parent_span: Range<usize>) -> Self {
        match item {
            toml_edit::Item::None => TomlValue {
                value: None,
                required: true,
                state: TomlValueState::Missing {
                    key: "".to_string(),
                    parent_span,
                },
            },
            toml_edit::Item::Value(toml_edit::Value::Float(formatted)) => {
                let value = *formatted.value();
                TomlValue {
                    required: true,
                    value: Some(value),
                    state: TomlValueState::Ok {
                        span: formatted.span().unwrap_or(parent_span.clone()),
                    },
                }
            }
            toml_edit::Item::Value(toml_edit::Value::Integer(formatted)) => {
                let value = *formatted.value() as f64;
                TomlValue {
                    required: true,
                    value: Some(value),
                    state: TomlValueState::Ok {
                        span: formatted.span().unwrap_or(parent_span.clone()),
                    },
                }
            }
            toml_edit::Item::Value(value) => TomlValue {
                required: true,
                value: None,
                state: TomlValueState::WrongType {
                    span: value.span().unwrap_or(parent_span.clone()),
                    expected: "f64",
                    found: value.type_name(),
                },
            },
            toml_edit::Item::Table(value) => TomlValue {
                required: true,
                value: None,
                state: TomlValueState::WrongType {
                    span: value.span().unwrap_or(parent_span.clone()),
                    expected: "f64",
                    found: "table",
                },
            },
            toml_edit::Item::ArrayOfTables(value) => TomlValue {
                required: true,
                value: None,
                state: TomlValueState::WrongType {
                    span: value.span().unwrap_or(parent_span.clone()),
                    expected: "f64",
                    found: "array of tables",
                },
            },
        }
    }
}

impl FromTomlItem for TomlValue<bool> {
    fn is_optional() -> bool {
        true
    }

    fn from_toml_item(item: &toml_edit::Item, parent_span: Range<usize>) -> Self {
        match item {
            toml_edit::Item::None => TomlValue {
                value: None,
                required: true,
                state: TomlValueState::Missing {
                    key: "".to_string(),
                    parent_span,
                },
            },
            toml_edit::Item::Value(toml_edit::Value::Boolean(formatted)) => {
                let value = *formatted.value();
                TomlValue {
                    required: true,
                    value: Some(value),
                    state: TomlValueState::Ok {
                        span: formatted.span().unwrap_or(parent_span.clone()),
                    },
                }
            }
            toml_edit::Item::Value(value) => TomlValue {
                required: true,
                value: None,
                state: TomlValueState::WrongType {
                    span: value.span().unwrap_or(parent_span.clone()),
                    expected: "bool",
                    found: value.type_name(),
                },
            },
            toml_edit::Item::Table(value) => TomlValue {
                required: true,
                value: None,
                state: TomlValueState::WrongType {
                    span: value.span().unwrap_or(parent_span.clone()),
                    expected: "bool",
                    found: "table",
                },
            },
            toml_edit::Item::ArrayOfTables(value) => TomlValue {
                required: true,
                value: None,
                state: TomlValueState::WrongType {
                    span: value.span().unwrap_or(parent_span.clone()),
                    expected: "bool",
                    found: "array of tables",
                },
            },
        }
    }
}

impl FromTomlItem for TomlValue<String> {
    fn is_optional() -> bool {
        true
    }

    fn from_toml_item(item: &toml_edit::Item, parent_span: Range<usize>) -> Self {
        match item {
            toml_edit::Item::None => TomlValue {
                value: None,
                required: true,
                state: TomlValueState::Missing {
                    key: "".to_string(),
                    parent_span,
                },
            },
            toml_edit::Item::Value(toml_edit::Value::String(formatted)) => {
                let value = formatted.value().to_string();
                TomlValue {
                    required: true,
                    value: Some(value),
                    state: TomlValueState::Ok {
                        span: formatted.span().unwrap_or(parent_span.clone()),
                    },
                }
            }
            toml_edit::Item::Value(value) => TomlValue {
                required: true,
                value: None,
                state: TomlValueState::WrongType {
                    span: value.span().unwrap_or(parent_span.clone()),
                    expected: "string",
                    found: value.type_name(),
                },
            },
            toml_edit::Item::Table(value) => TomlValue {
                required: true,
                value: None,
                state: TomlValueState::WrongType {
                    span: value.span().unwrap_or(parent_span.clone()),
                    expected: "string",
                    found: "table",
                },
            },
            toml_edit::Item::ArrayOfTables(value) => TomlValue {
                required: true,
                value: None,
                state: TomlValueState::WrongType {
                    span: value.span().unwrap_or(parent_span.clone()),
                    expected: "string",
                    found: "array of tables",
                },
            },
        }
    }
}

// Implementation for raw toml_edit::Item - used for nested struct deserialization
impl FromTomlItem for TomlValue<toml_edit::Item> {
    fn is_optional() -> bool {
        true
    }

    fn from_toml_item(item: &toml_edit::Item, parent_span: Range<usize>) -> Self {
        match item {
            toml_edit::Item::None => TomlValue {
                value: None,
                required: true,
                state: TomlValueState::Missing {
                    key: "".to_string(),
                    parent_span,
                },
            },
            toml_edit::Item::Table(table) => TomlValue {
                required: true,
                value: Some(toml_edit::Item::Table(table.clone())),
                state: TomlValueState::Ok {
                    span: table.span().unwrap_or(parent_span.clone()),
                },
            },
            toml_edit::Item::Value(toml_edit::Value::InlineTable(table)) => TomlValue {
                required: true,
                value: Some(toml_edit::Item::Value(toml_edit::Value::InlineTable(
                    table.clone(),
                ))),
                state: TomlValueState::Ok {
                    span: table.span().unwrap_or(parent_span.clone()),
                },
            },
            _ => TomlValue {
                required: true,
                value: Some(item.clone()),
                state: TomlValueState::Ok {
                    span: item.span().unwrap_or(parent_span.clone()),
                },
            },
        }
    }
}

impl FromTomlItem for TomlValue<Vec<toml_edit::Item>> {
    fn is_optional() -> bool {
        true
    }

    fn from_toml_item(item: &toml_edit::Item, parent_span: Range<usize>) -> Self {
        match item {
            toml_edit::Item::None => TomlValue {
                value: None,
                required: true,
                state: TomlValueState::Missing {
                    key: "".to_string(),
                    parent_span,
                },
            },
            toml_edit::Item::ArrayOfTables(array) => {
                let items: Vec<toml_edit::Item> = array
                    .iter()
                    .map(|table| toml_edit::Item::Table(table.clone()))
                    .collect();
                TomlValue {
                    required: true,
                    value: Some(items),
                    state: TomlValueState::Ok {
                        span: array.span().unwrap_or(parent_span.clone()),
                    },
                }
            }
            toml_edit::Item::Value(toml_edit::Value::Array(array)) => {
                // Also support regular arrays with inline tables
                let items: Vec<toml_edit::Item> = array
                    .iter()
                    .map(|val| toml_edit::Item::Value(val.clone()))
                    .collect();
                TomlValue {
                    required: true,
                    value: Some(items),
                    state: TomlValueState::Ok {
                        span: array.span().unwrap_or(parent_span.clone()),
                    },
                }
            }
            _ => TomlValue {
                required: true,
                value: None,
                state: TomlValueState::WrongType {
                    span: item.span().unwrap_or(parent_span.clone()),
                    expected: "array of tables",
                    found: "other type",
                },
            },
        }
    }
}

macro_rules! impl_from_toml_item_option {
    ($ty:ty) => {
        impl FromTomlItem for TomlValue<Option<$ty>> {
            fn is_optional() -> bool {
                false
            }

            fn from_toml_item(item: &toml_edit::Item, parent_span: Range<usize>) -> Self {
                let mut res: TomlValue<$ty> = FromTomlItem::from_toml_item(item, parent_span);
                res.required = false;
                match res.state {
                    TomlValueState::Ok { span } => TomlValue {
                        required: false,
                        value: Some(res.value),
                        state: TomlValueState::Ok { span },
                    },
                    TomlValueState::Missing { .. } => TomlValue {
                        required: false,
                        value: Some(None),
                        state: TomlValueState::Ok { span: 0..0 },
                    },
                    _ => TomlValue {
                        value: None,
                        required: false,
                        state: res.state,
                    },
                }
            }
        }
    };
}

impl_from_toml_item_option!(u8);
impl_from_toml_item_option!(i16);
impl_from_toml_item_option!(i32);
impl_from_toml_item_option!(i64);
impl_from_toml_item_option!(f64);
impl_from_toml_item_option!(bool);
impl_from_toml_item_option!(String);

// Special implementation for Option<toml_edit::Item> to support optional nested structs
impl FromTomlItem for TomlValue<Option<toml_edit::Item>> {
    fn is_optional() -> bool {
        false
    }

    fn from_toml_item(item: &toml_edit::Item, parent_span: Range<usize>) -> Self {
        let mut res: TomlValue<toml_edit::Item> = FromTomlItem::from_toml_item(item, parent_span);
        res.required = false;
        match res.state {
            TomlValueState::Ok { span } => TomlValue {
                required: false,
                value: Some(res.value),
                state: TomlValueState::Ok { span },
            },
            TomlValueState::Missing { .. } => TomlValue {
                required: false,
                value: Some(None),
                state: TomlValueState::Ok { span: 0..0 },
            },
            _ => TomlValue {
                value: None,
                required: false,
                state: res.state,
            },
        }
    }
}

macro_rules! impl_from_toml_item_vec {
    ($ty:ty, $name:expr) => {
        impl FromTomlItem for TomlValue<Vec<$ty>> {
            fn is_optional() -> bool {
                true
            }

            fn from_toml_item(item: &toml_edit::Item, parent_span: Range<usize>) -> Self {
                match item {
                    toml_edit::Item::None => TomlValue {
                        value: None,
                        required: true,
                        state: TomlValueState::Missing {
                            key: "".to_string(),
                            parent_span,
                        },
                    },
                    toml_edit::Item::Value(toml_edit::Value::Array(array)) => {
                        let mut values = Vec::with_capacity(array.len());
                        let mut has_error = false;

                        for item in array.iter() {
                            let item_span = item.span().unwrap_or(parent_span.clone());
                            let wrapped_item = toml_edit::Item::Value(item.clone());
                            let element: TomlValue<$ty> =
                                FromTomlItem::from_toml_item(&wrapped_item, item_span.clone());

                            match &element.state {
                                TomlValueState::Ok { .. } => {
                                    if let Some(val) = element.value {
                                        values.push(val);
                                    }
                                }
                                _ => {
                                    has_error = true;
                                }
                            }
                        }

                        if has_error {
                            TomlValue {
                                required: true,
                                value: None,
                                state: TomlValueState::ValidationFailed {
                                    span: array.span().unwrap_or(parent_span.clone()),
                                    message: "Array contains invalid elements".to_string(),
                                },
                            }
                        } else {
                            TomlValue {
                                required: true,
                                value: Some(values),
                                state: TomlValueState::Ok {
                                    span: array.span().unwrap_or(parent_span.clone()),
                                },
                            }
                        }
                    }
                    toml_edit::Item::Value(value) => TomlValue {
                        required: true,
                        value: None,
                        state: TomlValueState::WrongType {
                            span: value.span().unwrap_or(parent_span.clone()),
                            expected: "array",
                            found: value.type_name(),
                        },
                    },
                    toml_edit::Item::Table(value) => TomlValue {
                        required: true,
                        value: None,
                        state: TomlValueState::WrongType {
                            span: value.span().unwrap_or(parent_span.clone()),
                            expected: "array",
                            found: "table",
                        },
                    },
                    toml_edit::Item::ArrayOfTables(value) => TomlValue {
                        required: true,
                        value: None,
                        state: TomlValueState::WrongType {
                            span: value.span().unwrap_or(parent_span.clone()),
                            expected: "array",
                            found: "array of tables",
                        },
                    },
                }
            }
        }

        impl FromTomlItem for TomlValue<Option<Vec<$ty>>> {
            fn is_optional() -> bool {
                false
            }

            fn from_toml_item(item: &toml_edit::Item, parent_span: Range<usize>) -> Self {
                let mut res: TomlValue<Vec<$ty>> = FromTomlItem::from_toml_item(item, parent_span);
                res.required = false;
                match res.state {
                    TomlValueState::Ok { span } => TomlValue {
                        required: false,
                        value: Some(res.value),
                        state: TomlValueState::Ok { span },
                    },
                    TomlValueState::Missing { .. } => TomlValue {
                        required: false,
                        value: Some(None),
                        state: TomlValueState::Ok { span: 0..0 },
                    },
                    _ => TomlValue {
                        value: None,
                        required: false,
                        state: res.state,
                    },
                }
            }
        }
    };
}

impl_from_toml_item_vec!(u8, "u8");
impl_from_toml_item_vec!(i16, "i16");
impl_from_toml_item_vec!(i32, "i32");
impl_from_toml_item_vec!(i64, "i64");
impl_from_toml_item_vec!(f64, "f64");
impl_from_toml_item_vec!(bool, "bool");
impl_from_toml_item_vec!(String, "string");

#[derive(Debug, Clone)]
pub struct TomlValue<T> {
    pub value: Option<T>,
    pub(crate) state: TomlValueState,
    pub(crate) required: bool,
}

impl<T> Default for TomlValue<T> {
    fn default() -> Self {
        TomlValue {
            value: None,
            state: TomlValueState::NotSet,
            required: false,
        }
    }
}

impl<T> TomlValue<T> {
    pub fn has_value(&self) -> bool {
        matches!(self.state, TomlValueState::Ok { .. })
    }

    pub fn into_option(self) -> Option<T> {
        match self.state {
            TomlValueState::Ok { .. } => self.value,
            _ => None,
        }
    }

    pub fn as_ref(&self) -> Option<&T> {
        match self.state {
            TomlValueState::Ok { .. } => self.value.as_ref(),
            _ => None,
        }
    }

    pub fn unwrap(self) -> T {
        match self.state {
            TomlValueState::Ok { .. } => self.value.unwrap(),
            _ => panic!("Called unwrap on a TomlValue that is not Ok"),
        }
    }

    pub fn register_error(&self, errors: &Rc<RefCell<Vec<AnnotatedError>>>) {
        match &self.state {
            TomlValueState::NotSet => {}
            TomlValueState::Missing { key, parent_span } => {
                if self.required {
                    errors.borrow_mut().push(AnnotatedError::placed(
                        parent_span.clone(),
                        &format!("Missing required key: {}", key),
                        "This key is required but was not found in the TOML document.",
                    ));
                }
            }
            TomlValueState::WrongType {
                span,
                expected,
                found,
            } => {
                errors.borrow_mut().push(AnnotatedError::placed(
                    span.clone(),
                    &format!("Wrong type: expected {}, found {}", expected, found),
                    "The value has the wrong type.",
                ));
            }
            TomlValueState::ValidationFailed { span, message } => {
                errors.borrow_mut().push(AnnotatedError::placed(
                    span.clone(),
                    &format!("Validation failed: {}", message),
                    "The value failed validation checks.",
                ));
            }
            TomlValueState::Ok { .. } => {}
        }
    }

    pub fn verify<F>(self, verification_func: F) -> TomlValue<T>
    where
        F: FnOnce(&T) -> Result<(), String>,
    {
        match &self.state {
            TomlValueState::Ok { span } => match verification_func(self.value.as_ref().unwrap()) {
                Ok(()) => self,
                Err(msg) => TomlValue {
                    value: None,
                    required: self.required,
                    state: TomlValueState::ValidationFailed {
                        span: span.clone(),
                        message: msg,
                    },
                },
            },
            _ => self,
        }
    }

    pub fn or_default(self, default: T) -> TomlValue<T> {
        match &self.state {
            TomlValueState::Missing { .. } => TomlValue {
                value: Some(default),
                required: false,
                state: TomlValueState::Ok { span: 0..0 },
            },
            _ => self,
        }
    }
}

pub trait AsNested<P>: Sized {
    fn as_nested(
        self,
        errors: &Rc<RefCell<Vec<AnnotatedError>>>,
        mode: FieldMatchMode,
    ) -> TomlValue<P>;
}

impl<P> AsNested<P> for TomlValue<toml_edit::Item>
where
    P: FromTomlTable<()> + VerifyFromToml<()>,
{
    fn as_nested(
        self,
        errors: &Rc<RefCell<Vec<AnnotatedError>>>,
        mode: FieldMatchMode,
    ) -> TomlValue<P> {
        match &self.state {
            TomlValueState::Ok { span } => {
                if let Some(ref item) = self.value {
                    match item {
                        toml_edit::Item::Table(table) => {
                            let error_count_before = errors.borrow().len();
                            let mut helper = TomlHelper::new(table, errors.clone(), mode);
                            let partial =
                                P::from_toml_table(&mut helper, &()).verify(&mut helper, &());
                            helper.deny_unknown();

                            // Collect any new errors from the nested deserialization
                            let error_count_after = errors.borrow().len();

                            if error_count_after > error_count_before {
                                // There were errors during nested deserialization
                                TomlValue {
                                    value: Some(partial),
                                    required: self.required,
                                    state: TomlValueState::ValidationFailed {
                                        span: span.clone(),
                                        message: "Nested struct has errors".to_string(),
                                    },
                                }
                            } else {
                                TomlValue {
                                    value: Some(partial),
                                    required: self.required,
                                    state: TomlValueState::Ok { span: span.clone() },
                                }
                            }
                        }
                        toml_edit::Item::Value(toml_edit::Value::InlineTable(table)) => {
                            let error_count_before = errors.borrow().len();
                            let mut helper = TomlHelper::new_inline(table, errors.clone(), mode);
                            let partial =
                                P::from_toml_table(&mut helper, &()).verify(&mut helper, &());
                            helper.deny_unknown();

                            let error_count_after = errors.borrow().len();

                            if error_count_after > error_count_before {
                                TomlValue {
                                    value: Some(partial),
                                    required: self.required,
                                    state: TomlValueState::ValidationFailed {
                                        span: span.clone(),
                                        message: "Nested struct has errors".to_string(),
                                    },
                                }
                            } else {
                                TomlValue {
                                    value: Some(partial),
                                    required: self.required,
                                    state: TomlValueState::Ok { span: span.clone() },
                                }
                            }
                        }
                        _ => TomlValue {
                            value: None,
                            required: self.required,
                            state: TomlValueState::WrongType {
                                span: span.clone(),
                                expected: "table or inline table",
                                found: "other type",
                            },
                        },
                    }
                } else {
                    TomlValue {
                        value: None,
                        required: self.required,
                        state: TomlValueState::ValidationFailed {
                            span: span.clone(),
                            message: "Cannot convert empty value to nested struct".to_string(),
                        },
                    }
                }
            }
            TomlValueState::Missing { key, parent_span } => TomlValue {
                value: None,
                required: self.required,
                state: TomlValueState::Missing {
                    key: key.clone(),
                    parent_span: parent_span.clone(),
                },
            },
            _ => TomlValue {
                value: None,
                required: self.required,
                state: self.state.clone(),
            },
        }
    }
}

impl<P> AsNested<Option<P>> for TomlValue<Option<toml_edit::Item>>
where
    P: FromTomlTable<()> + VerifyFromToml<()>,
{
    fn as_nested(
        self,
        errors: &Rc<RefCell<Vec<AnnotatedError>>>,
        mode: FieldMatchMode,
    ) -> TomlValue<Option<P>> {
        match &self.state {
            TomlValueState::Ok { span } => {
                if let Some(ref opt_item) = self.value {
                    if let Some(item) = opt_item {
                        match item {
                            toml_edit::Item::Table(table) => {
                                let error_count_before = errors.borrow().len();
                                let mut helper = TomlHelper::new(table, errors.clone(), mode);
                                let partial =
                                    P::from_toml_table(&mut helper, &()).verify(&mut helper, &());
                                helper.deny_unknown();

                                let error_count_after = errors.borrow().len();

                                if error_count_after > error_count_before {
                                    TomlValue {
                                        value: Some(Some(partial)),
                                        required: self.required,
                                        state: TomlValueState::ValidationFailed {
                                            span: span.clone(),
                                            message: "Nested struct has errors".to_string(),
                                        },
                                    }
                                } else {
                                    TomlValue {
                                        value: Some(Some(partial)),
                                        required: self.required,
                                        state: TomlValueState::Ok { span: span.clone() },
                                    }
                                }
                            }
                            toml_edit::Item::Value(toml_edit::Value::InlineTable(table)) => {
                                let error_count_before = errors.borrow().len();
                                let mut helper =
                                    TomlHelper::new_inline(table, errors.clone(), mode);
                                let partial =
                                    P::from_toml_table(&mut helper, &()).verify(&mut helper, &());
                                helper.deny_unknown();

                                let error_count_after = errors.borrow().len();

                                if error_count_after > error_count_before {
                                    TomlValue {
                                        value: Some(Some(partial)),
                                        required: self.required,
                                        state: TomlValueState::ValidationFailed {
                                            span: span.clone(),
                                            message: "Nested struct has errors".to_string(),
                                        },
                                    }
                                } else {
                                    TomlValue {
                                        value: Some(Some(partial)),
                                        required: self.required,
                                        state: TomlValueState::Ok { span: span.clone() },
                                    }
                                }
                            }
                            _ => TomlValue {
                                value: None,
                                required: self.required,
                                state: TomlValueState::WrongType {
                                    span: span.clone(),
                                    expected: "table",
                                    found: "other type",
                                },
                            },
                        }
                    } else {
                        // None value is valid for Option<P>
                        TomlValue {
                            value: Some(None),
                            required: self.required,
                            state: TomlValueState::Ok { span: span.clone() },
                        }
                    }
                } else {
                    TomlValue {
                        value: None,
                        required: self.required,
                        state: TomlValueState::ValidationFailed {
                            span: span.clone(),
                            message: "Cannot convert empty value to nested struct".to_string(),
                        },
                    }
                }
            }
            TomlValueState::Missing {
                key: _,
                parent_span,
            } => TomlValue {
                value: Some(None),
                required: self.required,
                state: TomlValueState::Ok {
                    span: parent_span.clone(),
                },
            },
            _ => TomlValue {
                value: None,
                required: self.required,
                state: self.state.clone(),
            },
        }
    }
}

impl<P> AsNested<Vec<P>> for TomlValue<Vec<toml_edit::Item>>
where
    P: FromTomlTable<()> + VerifyFromToml<()>,
{
    fn as_nested(
        self,
        errors: &Rc<RefCell<Vec<AnnotatedError>>>,
        mode: FieldMatchMode,
    ) -> TomlValue<Vec<P>> {
        match &self.state {
            TomlValueState::Ok { span } => {
                if let Some(ref items) = self.value {
                    let mut results = Vec::with_capacity(items.len());
                    let error_count_before = errors.borrow().len();

                    for item in items.iter() {
                        match item {
                            toml_edit::Item::Table(table) => {
                                let item_error_count_before = errors.borrow().len();
                                let mut helper = TomlHelper::new(table, errors.clone(), mode);
                                let partial =
                                    P::from_toml_table(&mut helper, &()).verify(&mut helper, &());
                                helper.deny_unknown();

                                let item_error_count_after = errors.borrow().len();

                                if item_error_count_after > item_error_count_before {
                                    // There were errors during this item's deserialization
                                    // but we still push it to results so we can report all errors
                                    results.push(partial);
                                } else {
                                    results.push(partial);
                                }
                            }
                            _ => {
                                // Non-table item in array
                                errors.borrow_mut().push(AnnotatedError::placed(
                                    span.clone(),
                                    "Expected table in array",
                                    "Only table items are supported in nested struct arrays",
                                ));
                            }
                        }
                    }

                    let error_count_after = errors.borrow().len();

                    if error_count_after > error_count_before {
                        // There were errors during nested deserialization
                        TomlValue {
                            value: Some(results),
                            required: self.required,
                            state: TomlValueState::ValidationFailed {
                                span: span.clone(),
                                message: "Array of nested structs has errors".to_string(),
                            },
                        }
                    } else {
                        TomlValue {
                            value: Some(results),
                            required: self.required,
                            state: TomlValueState::Ok { span: span.clone() },
                        }
                    }
                } else {
                    TomlValue {
                        value: None,
                        required: self.required,
                        state: TomlValueState::ValidationFailed {
                            span: span.clone(),
                            message: "Cannot convert empty value to array of nested structs"
                                .to_string(),
                        },
                    }
                }
            }
            TomlValueState::Missing { key, parent_span } => TomlValue {
                value: None,
                required: self.required,
                state: TomlValueState::Missing {
                    key: key.clone(),
                    parent_span: parent_span.clone(),
                },
            },
            _ => TomlValue {
                value: None,
                required: self.required,
                state: self.state.clone(),
            },
        }
    }
}

#[derive(Debug, Clone)]
pub enum TomlValueState {
    NotSet,
    Missing {
        key: String,
        parent_span: Range<usize>,
    },
    WrongType {
        span: Range<usize>,
        expected: &'static str,
        found: &'static str,
    },
    ValidationFailed {
        span: Range<usize>,
        message: String,
    },
    Ok {
        span: Range<usize>,
    },
}
