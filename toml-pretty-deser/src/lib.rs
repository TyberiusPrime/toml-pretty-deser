use indexmap::{IndexMap, IndexSet};
use std::{cell::RefCell, ops::Range, rc::Rc};
use toml_edit::{Document, TomlError};

mod tablelike;
use tablelike::{AsTableLike, TableLikePlus};
pub use toml_pretty_deser_macros::{make_partial, make_partial_enum, StringNamedEnum};

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
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_normalize_no_case() {
        assert_eq!(normalize_to_no_case("snake_case"), "snakecase");
        assert_eq!(normalize_to_no_case("camelCase"), "camelcase");
        assert_eq!(normalize_to_no_case("UpperCamelCase"), "uppercamelcase");
        assert_eq!(normalize_to_no_case("kebab-case"), "kebabcase");
        assert_eq!(normalize_to_no_case("SHOUTY_SNAKE_CASE"), "shoutysnakecase");
        assert_eq!(normalize_to_no_case("Train-Case"), "traincase");
        assert_eq!(
            normalize_to_no_case("mixed_Case-Variant"),
            "mixedcasevariant"
        );
        assert_eq!(normalize_to_no_case("already_snake"), "alreadysnake");
        assert_eq!(normalize_to_no_case("HTMLParser"), "htmlparser");
        assert_eq!(normalize_to_no_case("getHTTPResponse"), "gethttpresponse");
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

fn convert_string_to_enum<E: StringNamedEnum>(s: &str, _span: &Range<usize>) -> Result<E, String> {
    match E::from_str(s) {
        Some(enum_val) => Ok(enum_val),
        None => Err(suggest_alternatives(s, E::all_variant_names())),
    }
}

impl<E: StringNamedEnum> AsEnum<E> for TomlValue<String> {
    fn as_enum(self) -> TomlValue<E> {
        match &self.state {
            TomlValueState::Ok { span } => match self.value {
                Some(s) => match convert_string_to_enum(&s, span) {
                    Ok(enum_val) => TomlValue {
                        value: Some(enum_val),
                        required: self.required,
                        state: TomlValueState::Ok { span: span.clone() },
                    },
                    Err(message) => TomlValue {
                        value: None,
                        required: self.required,
                        state: TomlValueState::ValidationFailed {
                            span: span.clone(),
                            message: "Invalid enum variant.".to_string(),
                            help: Some(message),
                        },
                    },
                },
                None => TomlValue {
                    value: None,
                    required: self.required,
                    state: TomlValueState::ValidationFailed {
                        span: span.clone(),
                        message: "Cannot convert empty value to enum".to_string(),
                        help: Some(suggest_alternatives("", E::all_variant_names())),
                    },
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
    #[mutants::skip]
    fn as_enum(self) -> TomlValue<Option<E>> {
        match &self.state {
            TomlValueState::Ok { span } => match self.value {
                Some(Some(s)) => match convert_string_to_enum(&s, span) {
                    Ok(enum_val) => TomlValue {
                        value: Some(Some(enum_val)),
                        required: self.required,
                        state: TomlValueState::Ok { span: span.clone() },
                    },
                    Err(message) => TomlValue {
                        value: None,
                        required: self.required,
                        state: TomlValueState::ValidationFailed {
                            span: span.clone(),
                            message: "Invalid enum variant.".to_string(),
                            help: Some(message),
                        },
                    },
                },
                Some(None) => TomlValue {
                    value: Some(None),
                    required: self.required,
                    state: TomlValueState::Ok { span: span.clone() },
                },
                None => TomlValue {
                    value: None,
                    required: self.required,
                    state: TomlValueState::ValidationFailed {
                        span: span.clone(),
                        message: "Cannot convert empty value to enum".to_string(),
                        help: Some(suggest_alternatives("", E::all_variant_names())),
                    },
                },
            },
            TomlValueState::Missing {
                key: _,
                parent_span: _,
            } => unreachable!(),
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
            TomlValueState::Ok { span } => match self.value {
                Some(strings) => {
                    let mut values = Vec::with_capacity(strings.len());
                    for s in &strings {
                        match convert_string_to_enum(s, span) {
                            Ok(enum_val) => values.push(enum_val),
                            Err(message) => {
                                return TomlValue {
                                    value: None,
                                    required: self.required,
                                    state: TomlValueState::ValidationFailed {
                                        span: span.clone(),
                                        message: "Invalid enum variant.".to_string(),
                                        help: Some(message),
                                    },
                                };
                            }
                        }
                    }
                    TomlValue {
                        value: Some(values),
                        required: self.required,
                        state: TomlValueState::Ok { span: span.clone() },
                    }
                }
                None => TomlValue {
                    value: None,
                    required: self.required,
                    state: TomlValueState::ValidationFailed {
                        span: span.clone(),
                        message: "Cannot convert empty value to enum".to_string(),
                        help: Some(suggest_alternatives("", E::all_variant_names())),
                    },
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
    #[mutants::skip]
    fn as_enum(self) -> TomlValue<Option<Vec<E>>> {
        match &self.state {
            TomlValueState::Ok { span } => match self.value {
                Some(Some(strings)) => {
                    let mut values = Vec::with_capacity(strings.len());
                    for s in strings {
                        match convert_string_to_enum(&s, span) {
                            Ok(enum_val) => values.push(enum_val),
                            Err(message) => {
                                return TomlValue {
                                    value: None,
                                    required: self.required,
                                    state: TomlValueState::ValidationFailed {
                                        span: span.clone(),
                                        help: Some(message),
                                        message: "Invalid enum variant.".to_string(),
                                    },
                                };
                            }
                        }
                    }
                    TomlValue {
                        value: Some(Some(values)),
                        required: self.required,
                        state: TomlValueState::Ok { span: span.clone() },
                    }
                }
                Some(None) => TomlValue {
                    value: Some(None),
                    required: self.required,
                    state: TomlValueState::Ok { span: span.clone() },
                },
                None => TomlValue {
                    value: None,
                    required: self.required,
                    state: TomlValueState::ValidationFailed {
                        span: span.clone(),
                        message: "Cannot convert empty value to enum".to_string(),
                        help: Some(suggest_alternatives("", E::all_variant_names())),
                    },
                },
            },
            TomlValueState::Missing {
                key: _,
                parent_span: _,
            } => unreachable!(),
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
    let mut helper = TomlHelper::from_table(parsed_toml.as_table(), errors.clone(), mode);

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

impl<P> From<TomlError> for DeserError<P> {
    fn from(value: TomlError) -> Self {
        DeserError::ParsingFailure(value)
    }
}

pub trait FromTomlTable<T> {
    fn can_concrete(&self) -> bool;
    fn collect_errors(&self, errors: &Rc<RefCell<Vec<AnnotatedError>>>);
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
    table: Option<&'a dyn TableLikePlus>,
    /// Expected field info (what we allow to see)
    expected: Vec<FieldInfo>,
    /// Normalized names that were actually observed (matched against table keys)
    observed: Vec<String>,
    /// Original field names that were allowed to the keys observed
    pub errors: Rc<RefCell<Vec<AnnotatedError>>>,
    pub match_mode: FieldMatchMode,
}

impl<'a> TomlHelper<'a> {
    pub fn from_table(
        table: &'a dyn TableLikePlus,
        errors: Rc<RefCell<Vec<AnnotatedError>>>,
        match_mode: FieldMatchMode,
    ) -> Self {
        Self {
            table: Some(table),
            expected: vec![],
            observed: vec![],
            errors,
            match_mode,
        }
    }

    /// Create a TomlHelper from a toml_edit::Item (either Table or InlineTable)
    pub fn from_item(
        item: &'a toml_edit::Item,
        errors: Rc<RefCell<Vec<AnnotatedError>>>,
        match_mode: FieldMatchMode,
    ) -> Self {
        match item.as_table_like_plus() {
            Some(table) => Self::from_table(table, errors, match_mode),
            _ => Self {
                table: None,
                expected: vec![],
                observed: vec![],
                errors,
                match_mode,
            },
        }
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
    /// Register a field with optional aliases
    pub fn ignore_field(&mut self, name: impl Into<String>) {
        let name: String = name.into();
        let field_info = FieldInfo::new(name.clone());
        self.expected.push(field_info);
        self.observed.push(name);
    }

    /// Find a key in the table that matches the given field name (considering aliases and match mode)
    fn find_matching_keys(
        &self,
        name: &str,
        aliases: &[&'static str],
    ) -> Vec<(String, toml_edit::Item)> {
        let mut result = Vec::new();
        let _normalized_target = self.match_mode.normalize(name);
        let candidates = std::iter::once(name.to_string())
            .chain(aliases.iter().map(|x| x.to_string()))
            .collect::<Vec<_>>();

        // Collect all table keys
        let table_keys: Vec<String> = if let Some(table) = self.table {
            table.iter().map(|(k, _)| k.to_string()).collect()
        } else {
            vec![]
        };

        // Try to find a match
        for table_key in &table_keys {
            for candidate in &candidates {
                if self.match_mode.matches(candidate, table_key) {
                    if let Some(table) = self.table {
                        if let Some(item) = table.get(table_key) {
                            result.push((table_key.clone(), item.clone()));
                            break;
                        }
                    }
                }
            }
        }

        result
    }

    pub fn get<T>(&mut self, key: &str) -> TomlValue<T>
    where
        T: FromTomlItem + std::fmt::Debug,
    {
        self.get_with_aliases(key, vec![])
    }

    pub fn get_with_aliases<T>(
        &mut self,
        query_key: &str,
        aliases: Vec<&'static str>,
    ) -> TomlValue<T>
    where
        T: FromTomlItem + std::fmt::Debug,
    {
        let parent_span = if let Some(table) = self.table {
            table.span().unwrap_or(0..0)
        } else {
            0..0
        };

        // Register this field as expected
        self.expect_field(query_key, &aliases);

        // Try to find a matching key (considering aliases and match mode)
        let found_keys = self.find_matching_keys(query_key, &aliases);

        match found_keys.len() {
            0 => {
                // No match found
                let mut res: TomlValue<T> =  //needs flexibility for required/non required
                    FromTomlItem::from_toml_item(&toml_edit::Item::None, parent_span.clone());
                if let TomlValueState::Missing { ref mut key, .. } = res.state {
                    if key.is_empty() {
                        *key = query_key.to_string().clone();
                    }
                }
                res
            }
            1 => {
                let (matched_key, item) = found_keys.iter().next().unwrap();
                let res: TomlValue<T> = FromTomlItem::from_toml_item(&item, parent_span);
                self.observed.push(self.match_mode.normalize(&matched_key));
                res
            }
            _ => {
                let spans = found_keys
                    .iter()
                    .map(|(matched_key, _item)| self.span_from_key(matched_key))
                    .collect();
                for (matched_key, _) in &found_keys {
                    self.observed.push(self.match_mode.normalize(matched_key));
                }

                let mut res: TomlValue<T> =
                    FromTomlItem::from_toml_item(&toml_edit::Item::None, parent_span);
                res.state = TomlValueState::MultiDefined {
                    key: query_key.to_string(),
                    spans,
                };
                res
            }
        }
    }

    fn span_from_key(&self, key: &str) -> Range<usize> {
        if let Some(table) = self.table {
            table.key(key).and_then(|item| item.span()).unwrap_or(0..0)
        // } else if let Some(inline_table) = self.inline_table {
        //     // For inline tables, we can't easily get the key span, so use the table span
        //     inline_table.span().unwrap_or(0..0)
        } else {
            0..0
        }
    }

    pub fn add_err_by_key(&self, key: &str, msg: &str, help: &str) {
        let span = self.span_from_key(key);
        self.add_err_by_span(span, msg, help);
    }

    pub fn add_err_by_span(&self, span: Range<usize>, msg: &str, help: &str) {
        let err = AnnotatedError::placed(span.clone(), msg, help);
        self.errors.borrow_mut().push(err);
    }

    pub fn deny_unknown(&self) {
        // Build set of normalized expected names (including aliases)
        let mut expected_normalized: IndexSet<String> = IndexSet::new();
        for field_info in &self.expected {
            for normalized_name in field_info.all_normalized_names(&self.match_mode) {
                expected_normalized.insert(normalized_name);
            }
        }

        // Collect all keys from either table type
        let keys: Vec<String> = if let Some(table) = self.table {
            table.iter().map(|(k, _)| k.to_string()).collect()
        } else {
            vec![]
        };

        // Build set of observed normalized names
        let observed_set: IndexSet<String> = self.observed.iter().cloned().collect();

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
                    "Unknown key.",
                    &suggest_alternatives(&key, &still_available),
                );
            }
        }
    }
}

pub trait FromTomlItem {
    fn from_toml_item(item: &toml_edit::Item, parent_span: Range<usize>) -> TomlValue<Self>
    where
        Self: Sized;
}

macro_rules! impl_from_toml_item_integer {
    ($ty:ty, $name:expr) => {
        impl FromTomlItem for $ty {
            fn from_toml_item(
                item: &toml_edit::Item,
                parent_span: Range<usize>,
            ) -> TomlValue<Self> {
                match item {
                    toml_edit::Item::None => TomlValue::new_empty_missing(parent_span),
                    toml_edit::Item::Value(toml_edit::Value::Integer(formatted)) => {
                        let value_i64 = *formatted.value();
                        if value_i64 < <$ty>::MIN as i64 || value_i64 > <$ty>::MAX as i64 {
                            TomlValue {
                                required: true,
                                value: None,
                                state: TomlValueState::ValidationFailed {
                                    span: formatted.span().unwrap_or(parent_span.clone()),
                                    message: "integer out of range.".to_string(),
                                    help: Some(format!("Accepted: {}..{}", <$ty>::MIN, <$ty>::MAX)),
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

macro_rules! impl_from_toml_item_value {
    ($ty:ty, $name:expr, $variant:ident) => {
        impl FromTomlItem for $ty {
            fn from_toml_item(
                item: &toml_edit::Item,
                parent_span: Range<usize>,
            ) -> TomlValue<Self> {
                match item {
                    toml_edit::Item::None => TomlValue::new_empty_missing(parent_span),
                    toml_edit::Item::Value(toml_edit::Value::$variant(formatted)) => {
                        let value = formatted.value();
                        TomlValue {
                            required: true,
                            value: Some(value.clone()),
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

impl_from_toml_item_value!(bool, "bool", Boolean);
impl_from_toml_item_value!(String, "String", String);
impl_from_toml_item_value!(f64, "Float", Float);

// Implementation for raw toml_edit::Item - used for nested struct deserialization
impl FromTomlItem for toml_edit::Item {
    fn from_toml_item(item: &toml_edit::Item, parent_span: Range<usize>) -> TomlValue<Self> {
        match item {
            toml_edit::Item::None => TomlValue::new_empty_missing(parent_span),

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

macro_rules! impl_from_toml_item_option {
    ($ty:ty) => {
        impl FromTomlItem for Option<$ty> {
            fn from_toml_item(
                item: &toml_edit::Item,
                parent_span: Range<usize>,
            ) -> TomlValue<Self> {
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
// implementation for Option<toml_edit::Item> to support optional nested structs
impl_from_toml_item_option!(toml_edit::Item);

// Blanket implementation for Vec<T> where T: FromTomlItem
// This handles both regular arrays and ArrayOfTables (for Vec<toml_edit::Item> specifically)
impl<T: FromTomlItem> FromTomlItem for Vec<T> {
    fn from_toml_item(item: &toml_edit::Item, parent_span: Range<usize>) -> TomlValue<Self> {
        match item {
            toml_edit::Item::None => TomlValue::new_empty_missing(parent_span),
            toml_edit::Item::Value(toml_edit::Value::Array(array)) => {
                let mut values = Vec::with_capacity(array.len());
                let mut has_error = false;

                for array_item in array.iter() {
                    let item_span = array_item.span().unwrap_or(parent_span.clone());
                    let wrapped_item = toml_edit::Item::Value(array_item.clone());
                    let element: TomlValue<T> =
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
                            help: None,
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
            // Handle ArrayOfTables - convert each table to an Item and deserialize
            toml_edit::Item::ArrayOfTables(array) => {
                let mut values = Vec::with_capacity(array.len());
                let mut has_error = false;

                for table in array.iter() {
                    let table_span = table.span().unwrap_or(parent_span.clone());
                    let wrapped_item = toml_edit::Item::Table(table.clone());
                    let element: TomlValue<T> =
                        FromTomlItem::from_toml_item(&wrapped_item, table_span.clone());

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
                            message: "Array of tables contains invalid elements".to_string(),
                            help: None,
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
        }
    }
}

// Blanket implementation for Option<Vec<T>> where T: FromTomlItem
impl<T: FromTomlItem> FromTomlItem for Option<Vec<T>> {
    fn from_toml_item(item: &toml_edit::Item, parent_span: Range<usize>) -> TomlValue<Self> {
        let mut res: TomlValue<Vec<T>> = FromTomlItem::from_toml_item(item, parent_span);
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

#[derive(Debug, Clone)]
pub struct TomlValue<T> {
    pub value: Option<T>,
    pub state: TomlValueState,
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
    pub fn new_ok(value: T, span: Range<usize>) -> Self {
        TomlValue {
            value: Some(value),
            required: true,
            state: TomlValueState::Ok { span },
        }
    }

    pub fn new_empty_missing(parent_span: Range<usize>) -> Self {
        TomlValue {
            value: None,
            required: true,
            state: TomlValueState::Missing {
                key: "".to_string(),
                parent_span,
            },
        }
    }
    pub fn new_validation_failed(
        span: Range<usize>,
        message: String,
        help: Option<String>,
    ) -> Self {
        TomlValue {
            value: None,
            required: true,
            state: TomlValueState::ValidationFailed {
                span,
                message,
                help,
            },
        }
    }

    pub fn new_wrong_type(
        item: &toml_edit::Item,
        parent_span: Range<usize>,
        expected: &'static str,
    ) -> Self {
        TomlValue {
            value: None,
            required: true,
            state: TomlValueState::WrongType {
                span: item.span().unwrap_or(parent_span),
                expected,
                found: item.type_name(),
            },
        }
    }

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
                        &format!("Missing required key: '{key}'."),
                        "This key is required but was not found in the TOML document.",
                    ));
                }
            }
            TomlValueState::MultiDefined { key, spans } => {
                let mut err = AnnotatedError::placed(
                    spans[0].clone(),
                    "Key/alias conflict (defined multiple times)",
                    &format!("Use only one of the keys involved. Canonical is '{}'", key),
                );
                for span in spans.iter().skip(1) {
                    err.add_span(span.clone(), "Also defined here");
                }
                errors.borrow_mut().push(err);
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
            TomlValueState::ValidationFailed {
                span,
                message,
                help,
            } => {
                errors.borrow_mut().push(AnnotatedError::placed(
                    span.clone(),
                    message,
                    help.as_ref().map(|x| x.as_str()).unwrap_or(""),
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
                        help: None, //todo
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

pub fn deserialize_nested<P>(
    item: &toml_edit::Item,
    span: &Range<usize>,
    errors: &Rc<RefCell<Vec<AnnotatedError>>>,
    mode: FieldMatchMode,
    required: bool,
    fields_to_ignore: &[&str],
) -> TomlValue<P>
where
    P: FromTomlTable<()> + VerifyFromToml<()>,
{
    match item.as_table_like_plus() {
        Some(table) => {
            let mut helper = TomlHelper::from_table(table, errors.clone(), mode);
            for f in fields_to_ignore {
                helper.ignore_field(*f);
            }
            let partial = P::from_toml_table(&mut helper, &()).verify(&mut helper, &());
            helper.deny_unknown();

            if partial.can_concrete() {
                TomlValue {
                    value: Some(partial),
                    required,
                    state: TomlValueState::Ok { span: span.clone() },
                }
            } else {
                TomlValue {
                    value: Some(partial),
                    required,
                    state: TomlValueState::ValidationFailed {
                        span: span.clone(),
                        message: "Nested struct has errors".to_string(),
                        help: None,
                    },
                }
            }
        }
        None => TomlValue {
            value: None,
            required,
            state: TomlValueState::WrongType {
                span: span.clone(),
                expected: "table or inline table",
                found: "other type",
            },
        },
    }
}

// Struct to hold tag key information for tagged enums
#[derive(Debug, Clone)]
pub struct TaggedEnumTag {
    pub tag_key: &'static str,
}

pub trait AsTaggedEnum<E>: Sized {
    fn as_tagged_enum(
        self,
        tag_key: &'static str,
        tag_aliases: Vec<&'static str>,
        errors: &Rc<RefCell<Vec<AnnotatedError>>>,
        mode: FieldMatchMode,
        deserialize_variant: fn(
            &str,
            &toml_edit::Item,
            &Rc<RefCell<Vec<AnnotatedError>>>,
            FieldMatchMode,
            &[&str],
        ) -> Option<E>,
    ) -> TomlValue<E>;
}

impl<E> AsTaggedEnum<E> for TomlValue<toml_edit::Item>
where
    E: StringNamedEnum,
{
    fn as_tagged_enum(
        self,
        tag_key: &'static str,
        tag_aliases: Vec<&'static str>,
        errors: &Rc<RefCell<Vec<AnnotatedError>>>,
        mode: FieldMatchMode,
        deserialize_variant: fn(
            &str,
            &toml_edit::Item,
            &Rc<RefCell<Vec<AnnotatedError>>>,
            FieldMatchMode,
            &[&str],
        ) -> Option<E>,
    ) -> TomlValue<E> {
        match &self.state {
            TomlValueState::Ok { span } => {
                if let Some(ref item) = self.value {
                    // Use TomlHelper to find the tag key with aliases and case matching
                    let mut helper = TomlHelper::from_item(item, errors.clone(), mode);
                    let tag_result: TomlValue<String> =
                        helper.get_with_aliases(tag_key, tag_aliases.clone());

                    // Build the list of fields to ignore (canonical key + all aliases)
                    let mut fields_to_ignore: Vec<&str> = vec![tag_key];
                    fields_to_ignore.extend(tag_aliases.iter().copied());

                    match &tag_result.state {
                        TomlValueState::Ok { .. } => {
                            // Successfully found the tag value
                            let tag_str = tag_result.value.as_ref().unwrap();
                            let variant_names = E::all_variant_names();
                            let mut matched_variant: Option<&str> = None;

                            for variant_name in variant_names {
                                if mode.matches(variant_name, tag_str) {
                                    matched_variant = Some(variant_name);
                                    break;
                                }
                            }

                            if let Some(variant_name) = matched_variant {
                                // Deserialize the specific variant using the provided function
                                match deserialize_variant(
                                    variant_name,
                                    item,
                                    errors,
                                    mode,
                                    &fields_to_ignore,
                                ) {
                                    Some(partial) => TomlValue {
                                        value: Some(partial),
                                        required: self.required,
                                        state: TomlValueState::Ok { span: span.clone() },
                                    },
                                    None => TomlValue {
                                        value: None,
                                        required: self.required,
                                        state: TomlValueState::ValidationFailed {
                                            span: span.clone(),
                                            message: "Failed to deserialize variant".to_string(),
                                            help: None,
                                        },
                                    },
                                }
                            } else {
                                TomlValue {
                                    value: None,
                                    required: self.required,
                                    state: TomlValueState::ValidationFailed {
                                        span: span.clone(),
                                        message: "Unknown enum variant".to_string(),
                                        help: Some(suggest_alternatives(tag_str, variant_names)),
                                    },
                                }
                            }
                        }
                        TomlValueState::MultiDefined { key: _, spans } => TomlValue {
                            value: None,
                            required: self.required,
                            state: TomlValueState::MultiDefined {
                                key: tag_key.to_string(),
                                spans: spans.to_vec(),
                            },
                        },
                        TomlValueState::WrongType {
                            span: wrong_span,
                            expected: _,
                            found,
                        } => TomlValue {
                            value: None,
                            required: self.required,
                            state: TomlValueState::ValidationFailed {
                                span: wrong_span.clone(),
                                message: format!("Wrong type: {}, expected string", found),
                                help: Some(suggest_alternatives("", E::all_variant_names())),
                            },
                        },
                        TomlValueState::Missing { .. } => TomlValue {
                            value: None,
                            required: self.required,
                            state: TomlValueState::ValidationFailed {
                                span: span.clone(),
                                message: format!("Missing required tag field: {}", tag_key),
                                help: Some(suggest_alternatives("", E::all_variant_names())),
                            },
                        },
                        _ => TomlValue {
                            value: None,
                            required: self.required,
                            state: tag_result.state.clone(),
                        },
                    }
                } else {
                    TomlValue {
                        value: None,
                        required: self.required,
                        state: TomlValueState::ValidationFailed {
                            span: span.clone(),
                            message: "Cannot convert empty value to tagged enum".to_string(),
                            help: Some(suggest_alternatives("", E::all_variant_names())),
                        },
                    }
                }
            }
            _ => TomlValue {
                value: None,
                required: self.required,
                state: self.state.clone(),
            },
        }
    }
}

pub trait AsOptionalTaggedEnum<E>: Sized {
    fn as_optional_tagged_enum(
        self,
        tag_key: &'static str,
        tag_aliases: Vec<&'static str>,
        errors: &Rc<RefCell<Vec<AnnotatedError>>>,
        mode: FieldMatchMode,
        deserialize_variant: fn(
            &str,
            &toml_edit::Item,
            &Rc<RefCell<Vec<AnnotatedError>>>,
            FieldMatchMode,
            &[&str],
        ) -> Option<E>,
    ) -> TomlValue<Option<E>>;
}

impl<E> AsOptionalTaggedEnum<E> for TomlValue<Option<toml_edit::Item>>
where
    E: StringNamedEnum,
{
    fn as_optional_tagged_enum(
        self,
        tag_key: &'static str,
        tag_aliases: Vec<&'static str>,
        errors: &Rc<RefCell<Vec<AnnotatedError>>>,
        mode: FieldMatchMode,
        deserialize_variant: fn(
            &str,
            &toml_edit::Item,
            &Rc<RefCell<Vec<AnnotatedError>>>,
            FieldMatchMode,
            &[&str],
        ) -> Option<E>,
    ) -> TomlValue<Option<E>> {
        match &self.state {
            TomlValueState::Ok { span } => match self.value {
                Some(Some(item)) => {
                    // Delegate to the single-item implementation
                    let single: TomlValue<toml_edit::Item> = TomlValue {
                        value: Some(item),
                        required: false, // Optional field
                        state: TomlValueState::Ok { span: span.clone() },
                    };
                    let result = single.as_tagged_enum(
                        tag_key,
                        tag_aliases,
                        errors,
                        mode,
                        deserialize_variant,
                    );
                    match result.state {
                        TomlValueState::Ok { span } => TomlValue {
                            value: Some(result.value),
                            required: self.required,
                            state: TomlValueState::Ok { span },
                        },
                        other => TomlValue {
                            value: None,
                            required: self.required,
                            state: other,
                        },
                    }
                }
                Some(None) => TomlValue {
                    value: Some(None),
                    required: self.required,
                    state: TomlValueState::Ok { span: span.clone() },
                },
                None => TomlValue {
                    value: None,
                    required: self.required,
                    state: TomlValueState::ValidationFailed {
                        span: span.clone(),
                        message: "Cannot convert empty value to optional tagged enum".to_string(),
                        help: Some(suggest_alternatives("", E::all_variant_names())),
                    },
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

pub trait AsVecTaggedEnum<E>: Sized {
    fn as_vec_tagged_enum(
        self,
        tag_key: &'static str,
        tag_aliases: Vec<&'static str>,
        errors: &Rc<RefCell<Vec<AnnotatedError>>>,
        mode: FieldMatchMode,
        deserialize_variant: fn(
            &str,
            &toml_edit::Item,
            &Rc<RefCell<Vec<AnnotatedError>>>,
            FieldMatchMode,
            &[&str],
        ) -> Option<E>,
    ) -> TomlValue<Vec<E>>;
}

impl<E> AsVecTaggedEnum<E> for TomlValue<Vec<toml_edit::Item>>
where
    E: StringNamedEnum,
{
    fn as_vec_tagged_enum(
        self,
        tag_key: &'static str,
        tag_aliases: Vec<&'static str>,
        errors: &Rc<RefCell<Vec<AnnotatedError>>>,
        mode: FieldMatchMode,
        deserialize_variant: fn(
            &str,
            &toml_edit::Item,
            &Rc<RefCell<Vec<AnnotatedError>>>,
            FieldMatchMode,
            &[&str],
        ) -> Option<E>,
    ) -> TomlValue<Vec<E>> {
        match &self.state {
            TomlValueState::Ok { span } => match self.value {
                Some(items) => {
                    let mut results: Vec<E> = Vec::with_capacity(items.len());
                    let mut has_errors = false;

                    for item in &items {
                        match item {
                            toml_edit::Item::Table(_)
                            | toml_edit::Item::Value(toml_edit::Value::InlineTable(_)) => {
                                let single: TomlValue<toml_edit::Item> = TomlValue {
                                    value: Some(item.clone()),
                                    required: true,
                                    state: TomlValueState::Ok { span: span.clone() },
                                };
                                let result = single.as_tagged_enum(
                                    tag_key,
                                    tag_aliases.clone(),
                                    errors,
                                    mode,
                                    deserialize_variant,
                                );
                                match result.value {
                                    Some(e) => results.push(e),
                                    None => has_errors = true,
                                }
                            }
                            _ => {
                                errors.borrow_mut().push(AnnotatedError::placed(
                                    span.clone(),
                                    &format!("Expected table in array - was {}", item.type_name()),
                                    "Only table items are supported in tagged enum arrays",
                                ));
                                has_errors = true;
                            }
                        }
                    }

                    if has_errors {
                        TomlValue {
                            value: Some(results),
                            required: self.required,
                            state: TomlValueState::ValidationFailed {
                                span: span.clone(),
                                message: "Array of tagged enums has errors".to_string(),
                                help: None,
                            },
                        }
                    } else {
                        TomlValue {
                            value: Some(results),
                            required: self.required,
                            state: TomlValueState::Ok { span: span.clone() },
                        }
                    }
                }
                None => TomlValue {
                    value: None,
                    required: self.required,
                    state: TomlValueState::ValidationFailed {
                        span: span.clone(),
                        message: "Cannot convert empty value to array of tagged enums".to_string(),
                        help: None,
                    },
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
                    deserialize_nested(item, span, errors, mode, self.required, &[])
                } else {
                    TomlValue {
                        value: None,
                        required: self.required,
                        state: TomlValueState::ValidationFailed {
                            span: span.clone(),
                            message: "Cannot convert empty value to nested struct".to_string(),
                            help: None,
                        },
                    }
                }
            }
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
            TomlValueState::Ok { span } => match self.value {
                Some(Some(item)) => {
                    let nested = deserialize_nested(&item, span, errors, mode, self.required, &[]);
                    match nested.value {
                        Some(partial) => TomlValue {
                            value: Some(Some(partial)),
                            required: self.required,
                            state: nested.state,
                        },
                        None => TomlValue {
                            value: None,
                            required: self.required,
                            state: nested.state,
                        },
                    }
                }
                Some(None) => TomlValue {
                    value: Some(None),
                    required: self.required,
                    state: TomlValueState::Ok { span: span.clone() },
                },
                None => TomlValue {
                    value: None,
                    required: self.required,
                    state: TomlValueState::ValidationFailed {
                        span: span.clone(),
                        message: "Cannot convert empty value to nested struct".to_string(),
                        help: None,
                    },
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
            TomlValueState::Ok { span } => match self.value {
                Some(items) => {
                    let mut results: Vec<P> = Vec::with_capacity(items.len());

                    for item in &items {
                        match item {
                            toml_edit::Item::Table(_)
                            | toml_edit::Item::Value(toml_edit::Value::InlineTable(_)) => {
                                let nested = deserialize_nested(
                                    item,
                                    span,
                                    errors,
                                    mode,
                                    self.required,
                                    &[],
                                );
                                if let Some(partial) = nested.value {
                                    results.push(partial);
                                } else {
                                    errors.borrow_mut().push(AnnotatedError::placed(
                                        span.clone(),
                                        &format!(
                                            "Expected table in array - was {}",
                                            item.type_name()
                                        ),
                                        "Only table items are supported in nested struct arrays",
                                    ));
                                }
                            }
                            _ => {
                                errors.borrow_mut().push(AnnotatedError::placed(
                                    span.clone(),
                                    &format!("Expected table in array - was {}", item.type_name()),
                                    "Only table items are supported in nested struct arrays",
                                ));
                            }
                        }
                    }

                    if results.iter().any(|partial| !partial.can_concrete()) {
                        TomlValue {
                            value: Some(results),
                            required: self.required,
                            state: TomlValueState::ValidationFailed {
                                span: span.clone(),
                                message: "Array of nested structs has errors".to_string(),
                                help: None,
                            },
                        }
                    } else {
                        TomlValue {
                            value: Some(results),
                            required: self.required,
                            state: TomlValueState::Ok { span: span.clone() },
                        }
                    }
                }
                None => TomlValue {
                    value: None,
                    required: self.required,
                    state: TomlValueState::ValidationFailed {
                        span: span.clone(),
                        message: "Cannot convert empty value to array of nested structs"
                            .to_string(),
                        help: None,
                    },
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

// ================================
// IndexMap / Map Support
// ================================

/// Trait for converting a TOML table into a IndexMap<String, T> where T is a primitive type
pub trait AsMap<T>: Sized {
    fn as_map(
        self,
        errors: &Rc<RefCell<Vec<AnnotatedError>>>,
        mode: FieldMatchMode,
    ) -> TomlValue<IndexMap<String, T>>;
}

/// Implementation for primitive types that implement FromTomlItem
macro_rules! impl_as_map_primitive {
    ($ty:ty) => {
        impl AsMap<$ty> for TomlValue<toml_edit::Item> {
            fn as_map(
                self,
                errors: &Rc<RefCell<Vec<AnnotatedError>>>,
                _mode: FieldMatchMode,
            ) -> TomlValue<IndexMap<String, $ty>> {
                match &self.state {
                    TomlValueState::Ok { span } => {
                        if let Some(ref item) = self.value {
                            match item.as_table_like_plus() {
                                Some(table) => {
                                    let mut map = IndexMap::new();
                                    let mut has_errors = false;

                                    for (key, value) in table.iter() {
                                        let item_span = value.span().unwrap_or(span.clone());
                                        let val: TomlValue<$ty> =
                                            FromTomlItem::from_toml_item(value, item_span.clone());
                                        match val.state {
                                            TomlValueState::Ok { .. } => {
                                                if let Some(v) = val.value {
                                                    map.insert(key.to_string(), v);
                                                }
                                            }
                                            _ => {
                                                val.register_error(errors);
                                                has_errors = true;
                                            }
                                        }
                                    }

                                    if has_errors {
                                        TomlValue {
                                            value: Some(map),
                                            required: self.required,
                                            state: TomlValueState::ValidationFailed {
                                                span: span.clone(),
                                                message: "Map contains invalid values".to_string(),
                                                help: None,
                                            },
                                        }
                                    } else {
                                        TomlValue {
                                            value: Some(map),
                                            required: self.required,
                                            state: TomlValueState::Ok { span: span.clone() },
                                        }
                                    }
                                }
                                None => TomlValue {
                                    value: None,
                                    required: self.required,
                                    state: TomlValueState::WrongType {
                                        span: span.clone(),
                                        expected: "table|inline_table",
                                        found: item.type_name(),
                                    },
                                },
                            }
                        } else {
                            TomlValue {
                                value: None,
                                required: self.required,
                                state: TomlValueState::ValidationFailed {
                                    span: span.clone(),
                                    message: "Cannot convert empty value to map".to_string(),
                                    help: None,
                                },
                            }
                        }
                    }
                    _ => TomlValue {
                        value: None,
                        required: self.required,
                        state: self.state.clone(),
                    },
                }
            }
        }

        impl AsMap<Option<$ty>> for TomlValue<Option<toml_edit::Item>> {
            fn as_map(
                self,
                errors: &Rc<RefCell<Vec<AnnotatedError>>>,
                mode: FieldMatchMode,
            ) -> TomlValue<IndexMap<String, Option<$ty>>> {
                match &self.state {
                    TomlValueState::Ok { span } => match self.value {
                        Some(Some(item)) => {
                            let single: TomlValue<toml_edit::Item> = TomlValue {
                                value: Some(item),
                                required: false,
                                state: TomlValueState::Ok { span: span.clone() },
                            };
                            let result: TomlValue<IndexMap<String, $ty>> =
                                single.as_map(errors, mode);
                            match result.state {
                                TomlValueState::Ok { span } => {
                                    let converted: IndexMap<String, Option<$ty>> = result
                                        .value
                                        .unwrap()
                                        .into_iter()
                                        .map(|(k, v)| (k, Some(v)))
                                        .collect();
                                    TomlValue {
                                        value: Some(converted),
                                        required: self.required,
                                        state: TomlValueState::Ok { span },
                                    }
                                }
                                other => TomlValue {
                                    value: None,
                                    required: self.required,
                                    state: other,
                                },
                            }
                        }
                        Some(None) => TomlValue {
                            value: Some(IndexMap::new()),
                            required: self.required,
                            state: TomlValueState::Ok { span: span.clone() },
                        },
                        None => TomlValue {
                            value: None,
                            required: self.required,
                            state: TomlValueState::ValidationFailed {
                                span: span.clone(),
                                message: "Cannot convert empty value to optional map".to_string(),
                                help: None,
                            },
                        },
                    },
                    TomlValueState::Missing { parent_span, .. } => TomlValue {
                        value: Some(IndexMap::new()),
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
    };
}

impl_as_map_primitive!(u8);
impl_as_map_primitive!(i16);
impl_as_map_primitive!(i32);
impl_as_map_primitive!(i64);
impl_as_map_primitive!(u32);
impl_as_map_primitive!(u64);
impl_as_map_primitive!(f64);
impl_as_map_primitive!(bool);
impl_as_map_primitive!(String);

/// Trait for converting a TOML table into a IndexMap<String, E> where E is a StringNamedEnum
pub trait AsMapEnum<E>: Sized {
    fn as_map_enum(
        self,
        errors: &Rc<RefCell<Vec<AnnotatedError>>>,
        mode: FieldMatchMode,
    ) -> TomlValue<IndexMap<String, E>>;
}

impl<E: StringNamedEnum> AsMapEnum<E> for TomlValue<toml_edit::Item> {
    fn as_map_enum(
        self,
        errors: &Rc<RefCell<Vec<AnnotatedError>>>,
        _mode: FieldMatchMode,
    ) -> TomlValue<IndexMap<String, E>> {
        match &self.state {
            TomlValueState::Ok { span } => {
                if let Some(ref item) = self.value {
                    match item.as_table_like_plus() {
                        Some(table) => {
                            let mut map = IndexMap::new();
                            let mut has_errors = false;

                            for (key, value) in table.iter() {
                                let item_span = value.span().unwrap_or(span.clone());
                                let val: TomlValue<String> =
                                    FromTomlItem::from_toml_item(value, item_span.clone());
                                let enum_val: TomlValue<E> = val.as_enum();
                                match enum_val.state {
                                    TomlValueState::Ok { .. } => {
                                        if let Some(v) = enum_val.value {
                                            map.insert(key.to_string(), v);
                                        }
                                    }
                                    _ => {
                                        enum_val.register_error(errors);
                                        has_errors = true;
                                    }
                                }
                            }

                            if has_errors {
                                TomlValue {
                                    value: Some(map),
                                    required: self.required,
                                    state: TomlValueState::ValidationFailed {
                                        span: span.clone(),
                                        message: "Map contains invalid enum values".to_string(),
                                        help: None,
                                    },
                                }
                            } else {
                                TomlValue {
                                    value: Some(map),
                                    required: self.required,
                                    state: TomlValueState::Ok { span: span.clone() },
                                }
                            }
                        }
                        None => TomlValue {
                            value: None,
                            required: self.required,
                            state: TomlValueState::WrongType {
                                span: span.clone(),
                                expected: "table|inline_table",
                                found: item.type_name(),
                            },
                        },
                    }
                } else {
                    TomlValue {
                        value: None,
                        required: self.required,
                        state: TomlValueState::ValidationFailed {
                            span: span.clone(),
                            message: "Cannot convert empty value to enum map".to_string(),
                            help: None,
                        },
                    }
                }
            }
            _ => TomlValue {
                value: None,
                required: self.required,
                state: self.state.clone(),
            },
        }
    }
}

/// Trait for converting a TOML table into a IndexMap<String, P> where P is a nested struct
pub trait AsMapNested<P>: Sized {
    fn as_map_nested(
        self,
        errors: &Rc<RefCell<Vec<AnnotatedError>>>,
        mode: FieldMatchMode,
    ) -> TomlValue<IndexMap<String, P>>;
}

impl<P> AsMapNested<P> for TomlValue<toml_edit::Item>
where
    P: FromTomlTable<()> + VerifyFromToml<()>,
{
    fn as_map_nested(
        self,
        errors: &Rc<RefCell<Vec<AnnotatedError>>>,
        mode: FieldMatchMode,
    ) -> TomlValue<IndexMap<String, P>> {
        match &self.state {
            TomlValueState::Ok { span } => {
                if let Some(ref item) = self.value {
                    match item.as_table_like_plus() {
                        Some(table) => {
                            let mut map = IndexMap::new();
                            let mut has_errors = false;

                            for (key, value) in table.iter() {
                                let item_span = value.span().unwrap_or(span.clone());
                                let nested: TomlValue<P> =
                                    deserialize_nested(value, &item_span, errors, mode, true, &[]);
                                match nested.state {
                                    TomlValueState::Ok { .. } => {
                                        if let Some(v) = nested.value {
                                            map.insert(key.to_string(), v);
                                        }
                                    }
                                    _ => {
                                        if let Some(value) = nested.value.as_ref() {
                                            value.collect_errors(&errors);
                                        }
                                        has_errors = true;
                                    }
                                }
                            }

                            if has_errors {
                                TomlValue {
                                    value: Some(map),
                                    required: self.required,
                                    state: TomlValueState::ValidationFailed {
                                        span: span.clone(),
                                        message: "Map contains invalid nested values".to_string(),
                                        help: None,
                                    },
                                }
                            } else {
                                TomlValue {
                                    value: Some(map),
                                    required: self.required,
                                    state: TomlValueState::Ok { span: span.clone() },
                                }
                            }
                        }
                        None => TomlValue {
                            value: None,
                            required: self.required,
                            state: TomlValueState::WrongType {
                                span: span.clone(),
                                expected: "table|inline_table",
                                found: item.type_name(),
                            },
                        },
                    }
                } else {
                    TomlValue {
                        value: None,
                        required: self.required,
                        state: TomlValueState::ValidationFailed {
                            span: span.clone(),
                            message: "Cannot convert empty value to nested map".to_string(),
                            help: None,
                        },
                    }
                }
            }
            _ => TomlValue {
                value: None,
                required: self.required,
                state: self.state.clone(),
            },
        }
    }
}

/// Trait for converting a TOML table into a IndexMap<String, E> where E is a tagged enum (externally tagged)
pub trait AsMapTaggedEnum<E>: Sized {
    fn as_map_tagged_enum(
        self,
        errors: &Rc<RefCell<Vec<AnnotatedError>>>,
        mode: FieldMatchMode,
        deserialize_variant: fn(
            &str,
            &toml_edit::Item,
            &Rc<RefCell<Vec<AnnotatedError>>>,
            FieldMatchMode,
            &[&str],
        ) -> Option<E>,
    ) -> TomlValue<IndexMap<String, E>>;
}

impl<E: StringNamedEnum> AsMapTaggedEnum<E> for TomlValue<toml_edit::Item> {
    fn as_map_tagged_enum(
        self,
        errors: &Rc<RefCell<Vec<AnnotatedError>>>,
        mode: FieldMatchMode,
        deserialize_variant: fn(
            &str,
            &toml_edit::Item,
            &Rc<RefCell<Vec<AnnotatedError>>>,
            FieldMatchMode,
            &[&str],
        ) -> Option<E>,
    ) -> TomlValue<IndexMap<String, E>> {
        match &self.state {
            TomlValueState::Ok { span } => {
                if let Some(ref item) = self.value {
                    match item.as_table_like_plus() {
                        Some(table) => {
                            let mut map = IndexMap::new();
                            let mut has_errors = false;

                            for (key, value) in table.iter() {
                                let item_span = value.span().unwrap_or(span.clone());
                                // Each value should be an inline table like { KindA = { ... } }
                                match value.as_table_like_plus() {
                                    Some(inline_table) => {
                                        // Find the variant key (should be exactly one)
                                        let mut found_variant = None;
                                        for (variant_key, variant_value) in inline_table.iter() {
                                            let variant_names = E::all_variant_names();
                                            for variant_name in variant_names {
                                                if mode.matches(variant_name, variant_key) {
                                                    let variant_item = variant_value.clone();
                                                    if let Some(partial) = deserialize_variant(
                                                        variant_name,
                                                        &variant_item,
                                                        errors,
                                                        mode,
                                                        &[],
                                                    ) {
                                                        found_variant = Some(partial);
                                                    }
                                                    break;
                                                }
                                            }
                                        }

                                        match found_variant {
                                            Some(variant) => {
                                                map.insert(key.to_string(), variant);
                                            }
                                            None => {
                                                errors.borrow_mut().push(AnnotatedError::placed(
                                                    item_span.clone(),
                                                    "Invalid or unknown tagged enum variant",
                                                    &suggest_alternatives(
                                                        "",
                                                        E::all_variant_names(),
                                                    ),
                                                ));
                                                has_errors = true;
                                            }
                                        }
                                    }
                                    None => {
                                        errors.borrow_mut().push(AnnotatedError::placed(
                                            item_span.clone(),
                                            "Expected table for tagged enum",
                                            "Value should be a table like { VariantName = { ... } }",
                                        ));
                                        has_errors = true;
                                    }
                                }
                            }

                            if has_errors {
                                TomlValue {
                                    value: Some(map),
                                    required: self.required,
                                    state: TomlValueState::ValidationFailed {
                                        span: span.clone(),
                                        message: "Map contains invalid tagged enum values"
                                            .to_string(),
                                        help: None,
                                    },
                                }
                            } else {
                                TomlValue {
                                    value: Some(map),
                                    required: self.required,
                                    state: TomlValueState::Ok { span: span.clone() },
                                }
                            }
                        }
                        None => TomlValue {
                            value: None,
                            required: self.required,
                            state: TomlValueState::WrongType {
                                span: span.clone(),
                                expected: "table|inline_table",
                                found: item.type_name(),
                            },
                        },
                    }
                } else {
                    TomlValue {
                        value: None,
                        required: self.required,
                        state: TomlValueState::ValidationFailed {
                            span: span.clone(),
                            message: "Cannot convert empty value to tagged enum map".to_string(),
                            help: None,
                        },
                    }
                }
            }
            _ => TomlValue {
                value: None,
                required: self.required,
                state: self.state.clone(),
            },
        }
    }
}

/// Trait for converting a TOML table into a IndexMap<String, Vec<T>>
pub trait AsMapVec<T>: Sized {
    fn as_map_vec(
        self,
        errors: &Rc<RefCell<Vec<AnnotatedError>>>,
        mode: FieldMatchMode,
    ) -> TomlValue<IndexMap<String, Vec<T>>>;
}

macro_rules! impl_as_map_vec_primitive {
    ($ty:ty) => {
        impl AsMapVec<$ty> for TomlValue<toml_edit::Item> {
            fn as_map_vec(
                self,
                errors: &Rc<RefCell<Vec<AnnotatedError>>>,
                _mode: FieldMatchMode,
            ) -> TomlValue<IndexMap<String, Vec<$ty>>> {
                match &self.state {
                    TomlValueState::Ok { span } => {
                        if let Some(ref item) = self.value {
                            match item.as_table_like_plus() {
                                Some(table) => {
                                    let mut map = IndexMap::new();
                                    let mut has_errors = false;

                                    for (key, value) in table.iter() {
                                        let item_span = value.span().unwrap_or(span.clone());
                                        let val: TomlValue<Vec<$ty>> =
                                            FromTomlItem::from_toml_item(value, item_span.clone());
                                        match val.state {
                                            TomlValueState::Ok { .. } => {
                                                if let Some(v) = val.value {
                                                    map.insert(key.to_string(), v);
                                                }
                                            }
                                            _ => {
                                                val.register_error(errors);
                                                has_errors = true;
                                            }
                                        }
                                    }

                                    if has_errors {
                                        TomlValue {
                                            value: Some(map),
                                            required: self.required,
                                            state: TomlValueState::ValidationFailed {
                                                span: span.clone(),
                                                message: "Map contains invalid vec values"
                                                    .to_string(),
                                                help: None,
                                            },
                                        }
                                    } else {
                                        TomlValue {
                                            value: Some(map),
                                            required: self.required,
                                            state: TomlValueState::Ok { span: span.clone() },
                                        }
                                    }
                                }
                                None => TomlValue {
                                    value: None,
                                    required: self.required,
                                    state: TomlValueState::WrongType {
                                        span: span.clone(),
                                        expected: "table|inline_table",
                                        found: item.type_name(),
                                    },
                                },
                            }
                        } else {
                            TomlValue {
                                value: None,
                                required: self.required,
                                state: TomlValueState::ValidationFailed {
                                    span: span.clone(),
                                    message: "Cannot convert empty value to vec map".to_string(),
                                    help: None,
                                },
                            }
                        }
                    }
                    _ => TomlValue {
                        value: None,
                        required: self.required,
                        state: self.state.clone(),
                    },
                }
            }
        }
    };
}

impl_as_map_vec_primitive!(u8);
impl_as_map_vec_primitive!(i16);
impl_as_map_vec_primitive!(i32);
impl_as_map_vec_primitive!(i64);
impl_as_map_vec_primitive!(u32);
impl_as_map_vec_primitive!(u64);
impl_as_map_vec_primitive!(f64);
impl_as_map_vec_primitive!(bool);
impl_as_map_vec_primitive!(String);

/// Trait for converting a TOML table into a IndexMap<String, Vec<E>> where E is a StringNamedEnum
pub trait AsMapVecEnum<E>: Sized {
    fn as_map_vec_enum(
        self,
        errors: &Rc<RefCell<Vec<AnnotatedError>>>,
        mode: FieldMatchMode,
    ) -> TomlValue<IndexMap<String, Vec<E>>>;
}

impl<E: StringNamedEnum> AsMapVecEnum<E> for TomlValue<toml_edit::Item> {
    fn as_map_vec_enum(
        self,
        errors: &Rc<RefCell<Vec<AnnotatedError>>>,
        _mode: FieldMatchMode,
    ) -> TomlValue<IndexMap<String, Vec<E>>> {
        match &self.state {
            TomlValueState::Ok { span } => {
                if let Some(ref item) = self.value {
                    match item.as_table_like_plus() {
                        Some(table) => {
                            let mut map = IndexMap::new();
                            let mut has_errors = false;

                            for (key, value) in table.iter() {
                                let item_span = value.span().unwrap_or(span.clone());
                                let val: TomlValue<Vec<String>> =
                                    FromTomlItem::from_toml_item(value, item_span.clone());
                                let enum_val: TomlValue<Vec<E>> = val.as_enum();
                                match enum_val.state {
                                    TomlValueState::Ok { .. } => {
                                        if let Some(v) = enum_val.value {
                                            map.insert(key.to_string(), v);
                                        }
                                    }
                                    _ => {
                                        enum_val.register_error(errors);
                                        has_errors = true;
                                    }
                                }
                            }

                            if has_errors {
                                TomlValue {
                                    value: Some(map),
                                    required: self.required,
                                    state: TomlValueState::ValidationFailed {
                                        span: span.clone(),
                                        message: "Map contains invalid vec enum values".to_string(),
                                        help: None,
                                    },
                                }
                            } else {
                                TomlValue {
                                    value: Some(map),
                                    required: self.required,
                                    state: TomlValueState::Ok { span: span.clone() },
                                }
                            }
                        }
                        None => TomlValue {
                            value: None,
                            required: self.required,
                            state: TomlValueState::WrongType {
                                span: span.clone(),
                                expected: "table|inline_table",
                                found: item.type_name(),
                            },
                        },
                    }
                } else {
                    TomlValue {
                        value: None,
                        required: self.required,
                        state: TomlValueState::ValidationFailed {
                            span: span.clone(),
                            message: "Cannot convert empty value to vec enum map".to_string(),
                            help: None,
                        },
                    }
                }
            }
            _ => TomlValue {
                value: None,
                required: self.required,
                state: self.state.clone(),
            },
        }
    }
}

/// Trait for converting a TOML table into a IndexMap<String, Vec<P>> where P is a nested struct
pub trait AsMapVecNested<P>: Sized {
    fn as_map_vec_nested(
        self,
        errors: &Rc<RefCell<Vec<AnnotatedError>>>,
        mode: FieldMatchMode,
    ) -> TomlValue<IndexMap<String, Vec<P>>>;
}

impl<P> AsMapVecNested<P> for TomlValue<toml_edit::Item>
where
    P: FromTomlTable<()> + VerifyFromToml<()>,
{
    fn as_map_vec_nested(
        self,
        errors: &Rc<RefCell<Vec<AnnotatedError>>>,
        mode: FieldMatchMode,
    ) -> TomlValue<IndexMap<String, Vec<P>>> {
        match &self.state {
            TomlValueState::Ok { span } => {
                if let Some(ref item) = self.value {
                    match item.as_table_like_plus() {
                        Some(table) => {
                            let mut map = IndexMap::new();
                            let mut has_errors = false;

                            for (key, value) in table.iter() {
                                let item_span = value.span().unwrap_or(span.clone());
                                // value should be an array
                                match value {
                                    toml_edit::Item::Value(toml_edit::Value::Array(array)) => {
                                        let mut vec = Vec::new();
                                        for array_item in array.iter() {
                                            let array_item_span =
                                                array_item.span().unwrap_or(item_span.clone());
                                            let item_as_toml =
                                                toml_edit::Item::Value(array_item.clone());
                                            let nested: TomlValue<P> = deserialize_nested(
                                                &item_as_toml,
                                                &array_item_span,
                                                errors,
                                                mode,
                                                true,
                                                &[],
                                            );
                                            match nested.state {
                                                TomlValueState::Ok { .. } => {
                                                    if let Some(v) = nested.value {
                                                        vec.push(v);
                                                    }
                                                }
                                                _ => {
                                                    nested
                                                        .value
                                                        .as_ref()
                                                        .expect("should not be none")
                                                        .collect_errors(&errors);
                                                    has_errors = true;
                                                }
                                            }
                                        }
                                        map.insert(key.to_string(), vec);
                                    }
                                    _ => {
                                        errors.borrow_mut().push(AnnotatedError::placed(
                                            item_span.clone(),
                                            "Expected array for map of vec nested",
                                            "Value should be an array of tables",
                                        ));
                                        has_errors = true;
                                    }
                                }
                            }

                            if has_errors {
                                TomlValue {
                                    value: Some(map),
                                    required: self.required,
                                    state: TomlValueState::ValidationFailed {
                                        span: span.clone(),
                                        message: "Map contains invalid vec nested values"
                                            .to_string(),
                                        help: None,
                                    },
                                }
                            } else {
                                TomlValue {
                                    value: Some(map),
                                    required: self.required,
                                    state: TomlValueState::Ok { span: span.clone() },
                                }
                            }
                        }
                        None => TomlValue {
                            value: None,
                            required: self.required,
                            state: TomlValueState::WrongType {
                                span: span.clone(),
                                expected: "table|inline_table",
                                found: item.type_name(),
                            },
                        },
                    }
                } else {
                    TomlValue {
                        value: None,
                        required: self.required,
                        state: TomlValueState::ValidationFailed {
                            span: span.clone(),
                            message: "Cannot convert empty value to vec nested map".to_string(),
                            help: None,
                        },
                    }
                }
            }
            _ => TomlValue {
                value: None,
                required: self.required,
                state: self.state.clone(),
            },
        }
    }
}

/// Trait for converting a TOML table into a IndexMap<String, Vec<E>> where E is a tagged enum
pub trait AsMapVecTaggedEnum<E>: Sized {
    fn as_map_vec_tagged_enum(
        self,
        errors: &Rc<RefCell<Vec<AnnotatedError>>>,
        mode: FieldMatchMode,
        deserialize_variant: fn(
            &str,
            &toml_edit::Item,
            &Rc<RefCell<Vec<AnnotatedError>>>,
            FieldMatchMode,
            &[&str],
        ) -> Option<E>,
    ) -> TomlValue<IndexMap<String, Vec<E>>>;
}

impl<E: StringNamedEnum> AsMapVecTaggedEnum<E> for TomlValue<toml_edit::Item> {
    fn as_map_vec_tagged_enum(
        self,
        errors: &Rc<RefCell<Vec<AnnotatedError>>>,
        mode: FieldMatchMode,
        deserialize_variant: fn(
            &str,
            &toml_edit::Item,
            &Rc<RefCell<Vec<AnnotatedError>>>,
            FieldMatchMode,
            &[&str],
        ) -> Option<E>,
    ) -> TomlValue<IndexMap<String, Vec<E>>> {
        match &self.state {
            TomlValueState::Ok { span } => {
                if let Some(ref item) = self.value {
                    match item.as_table_like_plus() {
                        Some(table) => {
                            let mut map = IndexMap::new();
                            let mut has_errors = false;

                            for (key, value) in table.iter() {
                                let item_span = value.span().unwrap_or(span.clone());
                                // value should be an array of inline tables
                                match value {
                                    toml_edit::Item::Value(toml_edit::Value::Array(array)) => {
                                        let mut vec = Vec::new();
                                        for array_item in array.iter() {
                                            let array_item_span =
                                                array_item.span().unwrap_or(item_span.clone());
                                            match array_item {
                                                toml_edit::Value::InlineTable(inline_table) => {
                                                    // Find the variant key (should be exactly one)
                                                    let mut found_variant = None;
                                                    for (variant_key, variant_value) in
                                                        inline_table.iter()
                                                    {
                                                        let variant_names = E::all_variant_names();
                                                        for variant_name in variant_names {
                                                            if mode
                                                                .matches(variant_name, variant_key)
                                                            {
                                                                let variant_item =
                                                                    toml_edit::Item::Value(
                                                                        variant_value.clone(),
                                                                    );
                                                                if let Some(partial) =
                                                                    deserialize_variant(
                                                                        variant_name,
                                                                        &variant_item,
                                                                        errors,
                                                                        mode,
                                                                        &[],
                                                                    )
                                                                {
                                                                    found_variant = Some(partial);
                                                                }
                                                                break;
                                                            }
                                                        }
                                                    }

                                                    match found_variant {
                                                        Some(variant) => {
                                                            vec.push(variant);
                                                        }
                                                        None => {
                                                            errors.borrow_mut().push(AnnotatedError::placed(
                                                                array_item_span.clone(),
                                                                "Invalid or unknown tagged enum variant",
                                                                &suggest_alternatives("", E::all_variant_names()),
                                                            ));
                                                            has_errors = true;
                                                        }
                                                    }
                                                }
                                                _ => {
                                                    errors.borrow_mut().push(AnnotatedError::placed(
                                                        array_item_span.clone(),
                                                        "Expected inline table for tagged enum",
                                                        "Value should be like { VariantName = { ... } }",
                                                    ));
                                                    has_errors = true;
                                                }
                                            }
                                        }
                                        map.insert(key.to_string(), vec);
                                    }
                                    _ => {
                                        errors.borrow_mut().push(AnnotatedError::placed(
                                            item_span.clone(),
                                            "Expected array for map of vec tagged enum",
                                            "Value should be an array of inline tables",
                                        ));
                                        has_errors = true;
                                    }
                                }
                            }

                            if has_errors {
                                TomlValue {
                                    value: Some(map),
                                    required: self.required,
                                    state: TomlValueState::ValidationFailed {
                                        span: span.clone(),
                                        message: "Map contains invalid vec tagged enum values"
                                            .to_string(),
                                        help: None,
                                    },
                                }
                            } else {
                                TomlValue {
                                    value: Some(map),
                                    required: self.required,
                                    state: TomlValueState::Ok { span: span.clone() },
                                }
                            }
                        }
                        None => TomlValue {
                            value: None,
                            required: self.required,
                            state: TomlValueState::WrongType {
                                span: span.clone(),
                                expected: "table|inline_table",
                                found: item.type_name(),
                            },
                        },
                    }
                } else {
                    TomlValue {
                        value: None,
                        required: self.required,
                        state: TomlValueState::ValidationFailed {
                            span: span.clone(),
                            message: "Cannot convert empty value to vec tagged enum map"
                                .to_string(),
                            help: None,
                        },
                    }
                }
            }
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
    MultiDefined {
        key: String,
        spans: Vec<Range<usize>>,
    },
    WrongType {
        span: Range<usize>,
        expected: &'static str,
        found: &'static str,
    },
    ValidationFailed {
        span: Range<usize>,
        message: String,
        help: Option<String>,
    },
    Ok {
        span: Range<usize>,
    },
}

// ================================
// Optional IndexMap Traits and Implementations
// These convert TomlValue<Option<Item>> to TomlValue<Option<IndexMap<String, T>>>
// ================================

/// Trait for converting an optional TOML table into Option<IndexMap<String, T>>
pub trait AsOptMap<T>: Sized {
    fn as_opt_map(
        self,
        errors: &Rc<RefCell<Vec<AnnotatedError>>>,
        mode: FieldMatchMode,
    ) -> TomlValue<Option<IndexMap<String, T>>>;
}

/// Trait for converting an optional TOML table into Option<IndexMap<String, E>> for string-named enums
pub trait AsOptMapEnum<E>: Sized {
    fn as_opt_map_enum(
        self,
        errors: &Rc<RefCell<Vec<AnnotatedError>>>,
        mode: FieldMatchMode,
    ) -> TomlValue<Option<IndexMap<String, E>>>;
}

/// Trait for converting an optional TOML table into Option<IndexMap<String, P>> for nested structs
pub trait AsOptMapNested<P>: Sized {
    fn as_opt_map_nested(
        self,
        errors: &Rc<RefCell<Vec<AnnotatedError>>>,
        mode: FieldMatchMode,
    ) -> TomlValue<Option<IndexMap<String, P>>>;
}

/// Trait for converting an optional TOML table into Option<IndexMap<String, E>> for tagged enums
pub trait AsOptMapTaggedEnum<E>: Sized {
    fn as_opt_map_tagged_enum(
        self,
        errors: &Rc<RefCell<Vec<AnnotatedError>>>,
        mode: FieldMatchMode,
        deserialize_variant: fn(
            &str,
            &toml_edit::Item,
            &Rc<RefCell<Vec<AnnotatedError>>>,
            FieldMatchMode,
            &[&str],
        ) -> Option<E>,
    ) -> TomlValue<Option<IndexMap<String, E>>>;
}

/// Trait for converting an optional TOML table into Option<IndexMap<String, Vec<T>>>
pub trait AsOptMapVec<T>: Sized {
    fn as_opt_map_vec(
        self,
        errors: &Rc<RefCell<Vec<AnnotatedError>>>,
        mode: FieldMatchMode,
    ) -> TomlValue<Option<IndexMap<String, Vec<T>>>>;
}

/// Trait for converting an optional TOML table into Option<IndexMap<String, Vec<E>>> for string-named enums
pub trait AsOptMapVecEnum<E>: Sized {
    fn as_opt_map_vec_enum(
        self,
        errors: &Rc<RefCell<Vec<AnnotatedError>>>,
        mode: FieldMatchMode,
    ) -> TomlValue<Option<IndexMap<String, Vec<E>>>>;
}

/// Trait for converting an optional TOML table into Option<IndexMap<String, Vec<P>>> for nested structs
pub trait AsOptMapVecNested<P>: Sized {
    fn as_opt_map_vec_nested(
        self,
        errors: &Rc<RefCell<Vec<AnnotatedError>>>,
        mode: FieldMatchMode,
    ) -> TomlValue<Option<IndexMap<String, Vec<P>>>>;
}

/// Trait for converting an optional TOML table into Option<IndexMap<String, Vec<E>>> for tagged enums
pub trait AsOptMapVecTaggedEnum<E>: Sized {
    fn as_opt_map_vec_tagged_enum(
        self,
        errors: &Rc<RefCell<Vec<AnnotatedError>>>,
        mode: FieldMatchMode,
        deserialize_variant: fn(
            &str,
            &toml_edit::Item,
            &Rc<RefCell<Vec<AnnotatedError>>>,
            FieldMatchMode,
            &[&str],
        ) -> Option<E>,
    ) -> TomlValue<Option<IndexMap<String, Vec<E>>>>;
}

/// Implementation for AsOptMap on Option<Item> returning Option<IndexMap<String, T>>
macro_rules! impl_as_opt_map_primitive {
    ($ty:ty) => {
        impl AsOptMap<$ty> for TomlValue<Option<toml_edit::Item>> {
            fn as_opt_map(
                self,
                errors: &Rc<RefCell<Vec<AnnotatedError>>>,
                mode: FieldMatchMode,
            ) -> TomlValue<Option<IndexMap<String, $ty>>> {
                match &self.state {
                    TomlValueState::Ok { span } => match self.value {
                        Some(Some(item)) => {
                            let single: TomlValue<toml_edit::Item> = TomlValue {
                                value: Some(item),
                                required: false,
                                state: TomlValueState::Ok { span: span.clone() },
                            };
                            let result: TomlValue<IndexMap<String, $ty>> =
                                single.as_map(errors, mode);
                            match result.state {
                                TomlValueState::Ok { span } => TomlValue {
                                    value: Some(result.value),
                                    required: self.required,
                                    state: TomlValueState::Ok { span },
                                },
                                other => TomlValue {
                                    value: None,
                                    required: self.required,
                                    state: other,
                                },
                            }
                        }
                        Some(None) => TomlValue {
                            value: Some(None),
                            required: self.required,
                            state: TomlValueState::Ok { span: span.clone() },
                        },
                        None => TomlValue {
                            value: Some(None),
                            required: self.required,
                            state: TomlValueState::Ok { span: span.clone() },
                        },
                    },
                    TomlValueState::Missing { parent_span, .. } => TomlValue {
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
    };
}

impl_as_opt_map_primitive!(u8);
impl_as_opt_map_primitive!(i16);
impl_as_opt_map_primitive!(i32);
impl_as_opt_map_primitive!(i64);
impl_as_opt_map_primitive!(u32);
impl_as_opt_map_primitive!(u64);
impl_as_opt_map_primitive!(f64);
impl_as_opt_map_primitive!(bool);
impl_as_opt_map_primitive!(String);

/// Implementation for AsOptMapEnum on Option<Item> returning Option<IndexMap<String, E>>
impl<E: StringNamedEnum> AsOptMapEnum<E> for TomlValue<Option<toml_edit::Item>> {
    fn as_opt_map_enum(
        self,
        errors: &Rc<RefCell<Vec<AnnotatedError>>>,
        mode: FieldMatchMode,
    ) -> TomlValue<Option<IndexMap<String, E>>> {
        match &self.state {
            TomlValueState::Ok { span } => match self.value {
                Some(Some(item)) => {
                    let single: TomlValue<toml_edit::Item> = TomlValue {
                        value: Some(item),
                        required: false,
                        state: TomlValueState::Ok { span: span.clone() },
                    };
                    let result: TomlValue<IndexMap<String, E>> = single.as_map_enum(errors, mode);
                    match result.state {
                        TomlValueState::Ok { span } => TomlValue {
                            value: Some(result.value),
                            required: self.required,
                            state: TomlValueState::Ok { span },
                        },
                        other => TomlValue {
                            value: None,
                            required: self.required,
                            state: other,
                        },
                    }
                }
                Some(None) => TomlValue {
                    value: Some(None),
                    required: self.required,
                    state: TomlValueState::Ok { span: span.clone() },
                },
                None => TomlValue {
                    value: Some(None),
                    required: self.required,
                    state: TomlValueState::Ok { span: span.clone() },
                },
            },
            TomlValueState::Missing { .. } => {
                unreachable!();
            }
            _ => TomlValue {
                value: None,
                required: self.required,
                state: self.state.clone(),
            },
        }
    }
}

/// Implementation for AsOptMapNested on Option<Item> returning Option<IndexMap<String, P>>
impl<P> AsOptMapNested<P> for TomlValue<Option<toml_edit::Item>>
where
    P: FromTomlTable<()> + VerifyFromToml<()>,
{
    fn as_opt_map_nested(
        self,
        errors: &Rc<RefCell<Vec<AnnotatedError>>>,
        mode: FieldMatchMode,
    ) -> TomlValue<Option<IndexMap<String, P>>> {
        match &self.state {
            TomlValueState::Ok { span } => match self.value {
                Some(Some(item)) => {
                    let single: TomlValue<toml_edit::Item> = TomlValue {
                        value: Some(item),
                        required: false,
                        state: TomlValueState::Ok { span: span.clone() },
                    };
                    let result: TomlValue<IndexMap<String, P>> = single.as_map_nested(errors, mode);
                    match result.state {
                        TomlValueState::Ok { span } => TomlValue {
                            value: Some(result.value),
                            required: self.required,
                            state: TomlValueState::Ok { span },
                        },
                        other => TomlValue {
                            value: None,
                            required: self.required,
                            state: other,
                        },
                    }
                }
                Some(None) => TomlValue {
                    value: Some(None),
                    required: self.required,
                    state: TomlValueState::Ok { span: span.clone() },
                },
                None => TomlValue {
                    value: Some(None),
                    required: self.required,
                    state: TomlValueState::Ok { span: span.clone() },
                },
            },
            TomlValueState::Missing { .. } => {
                unreachable!();
                // TomlValue {
                // value: Some(None),
                // required: self.required,
                // state: TomlValueState::Ok {
                //     span: parent_span.clone(),
                // },
            }
            _ => TomlValue {
                value: None,
                required: self.required,
                state: self.state.clone(),
            },
        }
    }
}

/// Implementation for AsOptMapTaggedEnum on Option<Item> returning Option<IndexMap<String, E>>
impl<E: StringNamedEnum> AsOptMapTaggedEnum<E> for TomlValue<Option<toml_edit::Item>> {
    fn as_opt_map_tagged_enum(
        self,
        errors: &Rc<RefCell<Vec<AnnotatedError>>>,
        mode: FieldMatchMode,
        deserialize_variant: fn(
            &str,
            &toml_edit::Item,
            &Rc<RefCell<Vec<AnnotatedError>>>,
            FieldMatchMode,
            &[&str],
        ) -> Option<E>,
    ) -> TomlValue<Option<IndexMap<String, E>>> {
        match &self.state {
            TomlValueState::Ok { span } => match self.value {
                Some(Some(item)) => {
                    let single: TomlValue<toml_edit::Item> = TomlValue {
                        value: Some(item),
                        required: false,
                        state: TomlValueState::Ok { span: span.clone() },
                    };
                    let result: TomlValue<IndexMap<String, E>> =
                        single.as_map_tagged_enum(errors, mode, deserialize_variant);
                    match result.state {
                        TomlValueState::Ok { span } => TomlValue {
                            value: Some(result.value),
                            required: self.required,
                            state: TomlValueState::Ok { span },
                        },
                        other => TomlValue {
                            value: None,
                            required: self.required,
                            state: other,
                        },
                    }
                }
                Some(None) => TomlValue {
                    value: Some(None),
                    required: self.required,
                    state: TomlValueState::Ok { span: span.clone() },
                },
                None => TomlValue {
                    value: Some(None),
                    required: self.required,
                    state: TomlValueState::Ok { span: span.clone() },
                },
            },
            TomlValueState::Missing { .. } => {
                unreachable!()
                // TomlValue {
                // value: Some(None),
                // required: self.required,
                // state: TomlValueState::Ok {
                //     span: parent_span.clone(),
                // },
            }
            _ => TomlValue {
                value: None,
                required: self.required,
                state: self.state.clone(),
            },
        }
    }
}

/// Implementation for AsOptMapVec on Option<Item> returning Option<IndexMap<String, Vec<T>>>
macro_rules! impl_as_opt_map_vec_primitive {
    ($ty:ty) => {
        impl AsOptMapVec<$ty> for TomlValue<Option<toml_edit::Item>> {
            fn as_opt_map_vec(
                self,
                errors: &Rc<RefCell<Vec<AnnotatedError>>>,
                mode: FieldMatchMode,
            ) -> TomlValue<Option<IndexMap<String, Vec<$ty>>>> {
                match &self.state {
                    TomlValueState::Ok { span } => match self.value {
                        Some(Some(item)) => {
                            let single: TomlValue<toml_edit::Item> = TomlValue {
                                value: Some(item),
                                required: false,
                                state: TomlValueState::Ok { span: span.clone() },
                            };
                            let result: TomlValue<IndexMap<String, Vec<$ty>>> =
                                single.as_map_vec(errors, mode);
                            match result.state {
                                TomlValueState::Ok { span } => TomlValue {
                                    value: Some(result.value),
                                    required: self.required,
                                    state: TomlValueState::Ok { span },
                                },
                                other => TomlValue {
                                    value: None,
                                    required: self.required,
                                    state: other,
                                },
                            }
                        }
                        Some(None) => TomlValue {
                            value: Some(None),
                            required: self.required,
                            state: TomlValueState::Ok { span: span.clone() },
                        },
                        None => TomlValue {
                            value: Some(None),
                            required: self.required,
                            state: TomlValueState::Ok { span: span.clone() },
                        },
                    },
                    TomlValueState::Missing { .. } => {
                        unreachable!();
                        // TomlValue {
                        //                 value: Some(None),
                        //                 required: self.required,
                        //                 state: TomlValueState::Ok {
                        //                     span: parent_span.clone(),
                        //                 },
                    }
                    _ => TomlValue {
                        value: None,
                        required: self.required,
                        state: self.state.clone(),
                    },
                }
            }
        }
    };
}

impl_as_opt_map_vec_primitive!(u8);
impl_as_opt_map_vec_primitive!(i16);
impl_as_opt_map_vec_primitive!(i32);
impl_as_opt_map_vec_primitive!(i64);
impl_as_opt_map_vec_primitive!(u32);
impl_as_opt_map_vec_primitive!(u64);
impl_as_opt_map_vec_primitive!(f64);
impl_as_opt_map_vec_primitive!(bool);
impl_as_opt_map_vec_primitive!(String);
