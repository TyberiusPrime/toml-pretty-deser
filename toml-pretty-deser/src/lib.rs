use indexmap::{IndexMap, IndexSet};
use std::{cell::RefCell, ops::Range, rc::Rc};
use toml_edit::{Document, TomlError};

pub mod prelude;
mod tablelike;
pub use tablelike::{AsTableLike, TableLikePlus};
pub use toml_pretty_deser_macros::{
    make_partial, tdp_make_enum, tdp_make_tagged_enum,
};

//needed to get the names from enums, implemented by tpd_make_enum
pub trait StringNamedEnum: Sized + Clone {
    fn all_variant_names() -> &'static [&'static str];
    fn from_str(s: &str) -> Option<Self>;
}

/// Trait for tagged enums that carry their tag key and aliases.
/// This is automatically implemented by `#[tdp_make_tagged_enum("tag_key")]`.
pub trait TaggedEnumMeta: Sized {
    /// The tag key used to identify the variant (e.g., "kind" or "type")
    const TAG_KEY: &'static str;
    /// Aliases for the tag key (e.g., if "type" is an alias for "kind")
    const TAG_ALIASES: &'static [&'static str];
    /// All variant names for error messages and matching
    fn all_variant_names() -> &'static [&'static str];
    /// Deserialize a specific variant by name from a TOML item
    fn deserialize_variant(
        variant_name: &str,
        item: &toml_edit::Item,
        col: &TomlCollector,
        fields_to_ignore: &[&str],
    ) -> Option<Self>;
}

/// Extension trait for deserializing tagged enums from TOML items.
/// This uses the `TaggedEnumMeta` trait to get tag key and aliases.
pub trait FromTaggedEnum<E: TaggedEnumMeta>: Sized {
    /// Deserialize a tagged enum from a TOML item
    fn from_tagged_enum(self, col: &TomlCollector) -> TomlValue<E>;
}

/// Extension trait for deserializing Vec of tagged enums from TOML items.
pub trait FromVecTaggedEnum<E: TaggedEnumMeta>: Sized {
    /// Deserialize a Vec of tagged enums from a TOML array
    fn from_vec_tagged_enum(self, col: &TomlCollector) -> TomlValue<Vec<E>>;
}

/// Extension trait for deserializing Vec of tagged enums with single-value support.
pub trait FromVecTaggedEnumAllowSingle<E: TaggedEnumMeta>: Sized {
    /// Deserialize a Vec of tagged enums, allowing a single value
    fn from_vec_tagged_enum_allow_single(self, col: &TomlCollector) -> TomlValue<Vec<E>>;
}

/// Extension trait for deserializing IndexMap of tagged enums.
pub trait FromMapTaggedEnum<E: TaggedEnumMeta>: Sized {
    /// Deserialize an IndexMap of tagged enums (using internal tagging)
    fn from_map_tagged_enum(self, col: &TomlCollector) -> TomlValue<IndexMap<String, E>>;
}

/// Extension trait for deserializing IndexMap of Vec of tagged enums.
pub trait FromMapVecTaggedEnum<E: TaggedEnumMeta>: Sized {
    /// Deserialize an IndexMap of Vec of tagged enums (using internal tagging)
    fn from_map_vec_tagged_enum(self, col: &TomlCollector) -> TomlValue<IndexMap<String, Vec<E>>>;
}

/// Extension trait for deserializing optional IndexMap of tagged enums.
pub trait FromOptMapTaggedEnum<E: TaggedEnumMeta>: Sized {
    /// Deserialize an optional IndexMap of tagged enums (using internal tagging)
    fn from_opt_map_tagged_enum(
        self,
        col: &TomlCollector,
    ) -> TomlValue<Option<IndexMap<String, E>>>;
}

/// Extension trait for deserializing optional IndexMap of Vec of tagged enums.
pub trait FromOptMapVecTaggedEnum<E: TaggedEnumMeta>: Sized {
    /// Deserialize an optional IndexMap of Vec of tagged enums (using internal tagging)
    fn from_opt_map_vec_tagged_enum(
        self,
        col: &TomlCollector,
    ) -> TomlValue<Option<IndexMap<String, Vec<E>>>>;
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

pub fn deserialize<P, T>(source: &str) -> Result<T, DeserError<P>>
where
    P: FromTomlTable<T> + VerifyFromToml<()>,
{
    deserialize_with_mode(source, FieldMatchMode::default())
}

pub fn deserialize_with_mode<P, T>(source: &str, mode: FieldMatchMode) -> Result<T, DeserError<P>>
where
    P: FromTomlTable<T> + VerifyFromToml<()>,
{
    let parsed_toml = source.parse::<Document<String>>()?;
    let source = Rc::new(RefCell::new(source.to_string()));

    let errors = Rc::new(RefCell::new(Vec::new()));
    let mut helper = TomlHelper::from_table(parsed_toml.as_table(), errors.clone(), mode);

    let partial = P::from_toml_table(&mut helper).verify(&mut helper, &());
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

pub fn suggest_alternatives<T: AsRef<str>>(current: &str, available: &[T]) -> String {
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

// /// Public helper for enum deserialization error messages.
// /// Used by the `#[tdp_make_enum]` macro to generate helpful error messages
// /// when an invalid enum variant is encountered.
pub fn suggest_enum_alternatives<E: StringNamedEnum>(current: &str) -> String {
    suggest_alternatives(current, E::all_variant_names())
}
//
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
    fn from_toml_table(helper: &mut TomlHelper<'_>) -> Self
    where
        Self: Sized;
    fn to_concrete(self) -> Option<T>;
}

pub trait VerifyFromToml<T> {
    fn verify(self, _helper: &mut TomlHelper<'_>, _partial: &T) -> Self
    where
        Self: Sized,
    {
        self
    }
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

pub struct HydratedAnnotatedError {
    pub source: Rc<RefCell<String>>,
    pub inner: AnnotatedError,
}

impl std::fmt::Debug for HydratedAnnotatedError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "HydratedAnnotatedError {{ ")?;
        write!(f, "{}", self.pretty("debug.toml"))?;
        writeln!(f, " }}")?;
        Ok(())
    }
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

    pub fn with_aliases(mut self, aliases: &'static[&'static str]) -> Self {
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

#[derive(Clone)]
pub struct TomlCollector {
    pub errors: Rc<RefCell<Vec<AnnotatedError>>>,
    pub match_mode: FieldMatchMode,
}

pub struct TomlHelper<'a> {
    table: Option<&'a dyn TableLikePlus>,
    /// Expected field info (what we allow to see)
    expected: Vec<FieldInfo>,
    /// Normalized names that were actually observed (matched against table keys)
    observed: Vec<String>,
    /// Original field names that were allowed to the keys observed
    col: TomlCollector,
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
            col: TomlCollector { errors, match_mode },
        }
    }

    /// Create a TomlHelper from a toml_edit::Item (either Table or InlineTable)
    pub fn from_item(item: &'a toml_edit::Item, col: &TomlCollector) -> Self {
        match item.as_table_like_plus() {
            Some(table) => Self::from_table(table, col.errors.clone(), col.match_mode),
            _ => Self {
                table: None,
                expected: vec![],
                observed: vec![],
                col: col.clone(),
            },
        }
    }

    pub fn into_inner(self, source: &Rc<RefCell<String>>) -> Vec<HydratedAnnotatedError> {
        self.col
            .errors
            .borrow_mut()
            .drain(..)
            .map(|error| HydratedAnnotatedError {
                source: source.clone(),
                inner: error,
            })
            .collect()
    }

    /// Register a field with optional aliases
    pub fn expect_field(&mut self, name: impl Into<String>, aliases: &'static [&'static str]) {
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
        let _normalized_target = self.col.match_mode.normalize(name);
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
                if self.col.match_mode.matches(candidate, table_key) {
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

    pub fn get_with_aliases<T>(
        &mut self,
        query_key: &str,
        aliases: &'static [&'static str],
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
        self.expect_field(query_key, aliases);

        // Try to find a matching key (considering aliases and match mode)
        let found_keys = self.find_matching_keys(query_key, &aliases);

        match found_keys.len() {
            0 => {
                // No match found
                let mut res: TomlValue<T> =  //needs flexibility for required/non required
                    FromTomlItem::from_toml_item(&toml_edit::Item::None, parent_span.clone(), &self.col);
                if let TomlValueState::Missing { ref mut key, .. } = res.state {
                    if key.is_empty() {
                        *key = query_key.to_string().clone();
                    }
                }
                res
            }
            1 => {
                let (matched_key, item) = found_keys.iter().next().unwrap();
                let res: TomlValue<T> = FromTomlItem::from_toml_item(&item, parent_span, &self.col);
                self.observed
                    .push(self.col.match_mode.normalize(&matched_key));
                res
            }
            _ => {
                let spans = found_keys
                    .iter()
                    .map(|(matched_key, _item)| self.span_from_key(matched_key))
                    .collect();
                for (matched_key, _) in &found_keys {
                    self.observed
                        .push(self.col.match_mode.normalize(matched_key));
                }

                let mut res: TomlValue<T> =
                    FromTomlItem::from_toml_item(&toml_edit::Item::None, parent_span, &self.col);
                res.state = TomlValueState::MultiDefined {
                    key: query_key.to_string(),
                    spans,
                };
                res
            }
        }
    }

    /// Get a `Vec<T>` field with aliases that allows either an array `[x, y, z]` or a single value `x`.
    /// If a single value is provided, it will be wrapped in a `Vec` of length 1.
    pub fn get_allow_single_with_aliases<T>(
        &mut self,
        query_key: &str,
        aliases: &'static [&'static str],
    ) -> TomlValue<Vec<T>>
    where
        T: FromTomlItem + std::fmt::Debug,
    {
        let parent_span = if let Some(table) = self.table {
            table.span().unwrap_or(0..0)
        } else {
            0..0
        };

        // Register this field as expected
        self.expect_field(query_key, aliases);

        // Try to find a matching key (considering aliases and match mode)
        let found_keys = self.find_matching_keys(query_key, &aliases);

        match found_keys.len() {
            0 => {
                // No match found - return a Missing state
                TomlValue {
                    required: true,
                    value: None,
                    state: TomlValueState::Missing {
                        key: query_key.to_string(),
                        parent_span: parent_span.clone(),
                    },
                }
            }
            1 => {
                let (matched_key, item) = found_keys.iter().next().unwrap();
  if matched_key == "opt_nested" {
                    panic!("{}", &matched_key, );
                }
                self.observed
                    .push(self.col.match_mode.normalize(&matched_key));

                // Check if the item is an array or a single value
                match item {
                    toml_edit::Item::Value(toml_edit::Value::Array(_)) => {
                        // It's an array, parse normally as Vec<T>
                        FromTomlItem::from_toml_item(&item, parent_span, &self.col)
                    }
                    toml_edit::Item::ArrayOfTables(_) => { //todo fold with above
                        // It's an array of tables, parse normally as Vec<T>
                        FromTomlItem::from_toml_item(&item, parent_span, &self.col)
                    }
                    _ => {
                        // It's a single value, try to parse as T and wrap in Vec
                        let item_span = item.span().unwrap_or(parent_span.clone());
                        let single: TomlValue<T> =
                            FromTomlItem::from_toml_item(&item, item_span.clone(), &self.col);
                        match single.state {
                            TomlValueState::Ok { span } => TomlValue {
                                required: true,
                                value: single.value.map(|v| vec![v]),
                                state: TomlValueState::Ok { span },
                            },
                            other => TomlValue {
                                required: true,
                                value: None,
                                state: other,
                            },
                        }
                    }
                }
            }
            _ => {
                let spans = found_keys
                    .iter()
                    .map(|(matched_key, _item)| self.span_from_key(matched_key))
                    .collect();
                for (matched_key, _) in &found_keys {
                    self.observed
                        .push(self.col.match_mode.normalize(matched_key));
                }

                TomlValue {
                    required: true,
                    value: None,
                    state: TomlValueState::MultiDefined {
                        key: query_key.to_string(),
                        spans,
                    },
                }
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
        self.col.errors.borrow_mut().push(err);
    }

    pub fn deny_unknown(&self) {
        // Build set of normalized expected names (including aliases)
        let mut expected_normalized: IndexSet<String> = IndexSet::new();
        for field_info in &self.expected {
            for normalized_name in field_info.all_normalized_names(&self.col.match_mode) {
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
            let normalized_key = self.col.match_mode.normalize(&key);

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
    fn from_toml_item(
        item: &toml_edit::Item,
        parent_span: Range<usize>,
        col: &TomlCollector,
    ) -> TomlValue<Self>
    where
        Self: Sized;
}

macro_rules! impl_from_toml_item_integer {
    ($ty:ty, $name:expr) => {
        impl FromTomlItem for $ty {
            fn from_toml_item(
                item: &toml_edit::Item,
                parent_span: Range<usize>,
                _col: &TomlCollector,
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
                _col: &TomlCollector,
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
    fn from_toml_item(
        item: &toml_edit::Item,
        parent_span: Range<usize>,
        _col: &TomlCollector,
    ) -> TomlValue<Self> {
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

// Blanket implementation for Option<T> where T: FromTomlItem
impl<T: FromTomlItem> FromTomlItem for Option<T> {
    fn from_toml_item(
        item: &toml_edit::Item,
        parent_span: Range<usize>,
        col: &TomlCollector,
    ) -> TomlValue<Self> {
        let mut res: TomlValue<T> = FromTomlItem::from_toml_item(item, parent_span, col);
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
                value: Some(res.value),
                required: false,
                state: res.state,
            },
        }
    }
}

// Blanket implementation for Vec<T> where T: FromTomlItem
// This handles both regular arrays and ArrayOfTables (for Vec<toml_edit::Item> specifically)
impl<T: FromTomlItem> FromTomlItem for Vec<T> {
    fn from_toml_item(
        item: &toml_edit::Item,
        parent_span: Range<usize>,
        col: &TomlCollector,
    ) -> TomlValue<Self> {
        match item {
            toml_edit::Item::None => TomlValue::new_empty_missing(parent_span),
            toml_edit::Item::Value(toml_edit::Value::Array(array)) => {
                let mut values = Vec::with_capacity(array.len());
                let mut has_error = false;

                for array_item in array.iter() {
                    let item_span = array_item.span().unwrap_or(parent_span.clone());
                    let wrapped_item = toml_edit::Item::Value(array_item.clone());
                    let element: TomlValue<T> =
                        FromTomlItem::from_toml_item(&wrapped_item, item_span.clone(), col);

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
                        FromTomlItem::from_toml_item(&wrapped_item, table_span.clone(), col);

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

#[derive(Debug, Clone)]
pub struct TomlValue<T> {
    pub value: Option<T>,
    pub state: TomlValueState,
    pub required: bool,
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

    pub fn into_optional(self) -> TomlValue<Option<T>> {
        match self.state {
            TomlValueState::Ok { span } => TomlValue {
                required: false,
                value: Some(self.value),
                state: TomlValueState::Ok { span },
            },
            TomlValueState::Missing {
                key: _,
                parent_span,
            } => TomlValue {
                required: false,
                value: Some(None),
                state: TomlValueState::Ok { span: parent_span },
            },
            _ => TomlValue {
                value: None,
                required: false,
                state: self.state,
            },
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

pub fn deserialize_nested<P, T>(
    item: &toml_edit::Item,
    span: &Range<usize>,
    col: &TomlCollector,
    required: bool,
    fields_to_ignore: &[&str],
) -> TomlValue<P>
where
    P: FromTomlTable<T> + VerifyFromToml<()>,
{
    match item.as_table_like_plus() {
        Some(table) => {
            let mut helper = TomlHelper::from_table(table, col.errors.clone(), col.match_mode);
            for f in fields_to_ignore {
                helper.ignore_field(*f);
            }
            let partial = P::from_toml_table(&mut helper).verify(&mut helper, &());
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

// ============================================================================
// NEW TaggedEnumMeta-based implementations
// ============================================================================




impl<E: TaggedEnumMeta> FromTaggedEnum<E> for TomlValue<toml_edit::Item> {
    fn from_tagged_enum(self, col: &TomlCollector) -> TomlValue<E> {
        let tag_key = E::TAG_KEY;
        let tag_aliases = E::TAG_ALIASES;

        match &self.state {
            TomlValueState::Ok { span } => {
                if let Some(ref item) = self.value {
                    // Use TomlHelper to find the tag key with aliases and case matching
                    let mut helper = TomlHelper::from_item(item, col);
                    let tag_result: TomlValue<String> =
                        helper.get_with_aliases(tag_key, tag_aliases);

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
                                if col.match_mode.matches(variant_name, tag_str) {
                                    matched_variant = Some(variant_name);
                                    break;
                                }
                            }

                            if let Some(variant_name) = matched_variant {
                                // Deserialize the specific variant
                                match E::deserialize_variant(
                                    variant_name,
                                    item,
                                    col,
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

impl<E: TaggedEnumMeta> FromVecTaggedEnum<E> for TomlValue<Vec<toml_edit::Item>> {
    fn from_vec_tagged_enum(self, col: &TomlCollector) -> TomlValue<Vec<E>> {
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
                                let result = single.from_tagged_enum(col);
                                match result.value {
                                    Some(e) => results.push(e),
                                    None => has_errors = true,
                                }
                            }
                            _ => {
                                col.errors.borrow_mut().push(AnnotatedError::placed(
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
                        message: "Cannot convert empty value to tagged enum array".to_string(),
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

impl<E: TaggedEnumMeta> FromVecTaggedEnumAllowSingle<E> for TomlValue<toml_edit::Item> {
    fn from_vec_tagged_enum_allow_single(self, col: &TomlCollector) -> TomlValue<Vec<E>> {
        let required = self.required;
        match &self.state {
            TomlValueState::Ok { span } => {
                if let Some(ref item) = self.value {
                    match item {
                        // Single table - wrap in vec
                        toml_edit::Item::Table(_)
                        | toml_edit::Item::Value(toml_edit::Value::InlineTable(_)) => {
                            let single_result = self.from_tagged_enum(col);
                            match single_result.state {
                                TomlValueState::Ok { span: s } => TomlValue {
                                    value: single_result.value.map(|e| vec![e]),
                                    required,
                                    state: TomlValueState::Ok { span: s },
                                },
                                other => TomlValue {
                                    value: None,
                                    required,
                                    state: other,
                                },
                            }
                        }
                        // Array of tables
                        toml_edit::Item::ArrayOfTables(arr) => {
                            let mut results: Vec<E> = Vec::with_capacity(arr.len());
                            let mut has_errors = false;

                            for table in arr.iter() {
                                let table_item = toml_edit::Item::Table(table.clone());
                                let item_span = table.span().unwrap_or(span.clone());
                                let single: TomlValue<toml_edit::Item> = TomlValue {
                                    value: Some(table_item),
                                    required: true,
                                    state: TomlValueState::Ok { span: item_span },
                                };
                                let result = single.from_tagged_enum(col);
                                match result.value {
                                    Some(e) => results.push(e),
                                    None => has_errors = true,
                                }
                            }

                            if has_errors {
                                TomlValue {
                                    value: Some(results),
                                    required,
                                    state: TomlValueState::ValidationFailed {
                                        span: span.clone(),
                                        message: "Array of tagged enums has errors".to_string(),
                                        help: None,
                                    },
                                }
                            } else {
                                TomlValue {
                                    value: Some(results),
                                    required,
                                    state: TomlValueState::Ok { span: span.clone() },
                                }
                            }
                        }
                        // Regular array (with inline tables inside)
                        toml_edit::Item::Value(toml_edit::Value::Array(arr)) => {
                            let mut results: Vec<E> = Vec::with_capacity(arr.len());
                            let mut has_errors = false;

                            for val in arr.iter() {
                                let item_span = val.span().unwrap_or(span.clone());
                                match val {
                                    toml_edit::Value::InlineTable(_) => {
                                        let item = toml_edit::Item::Value(val.clone());
                                        let single: TomlValue<toml_edit::Item> = TomlValue {
                                            value: Some(item),
                                            required: true,
                                            state: TomlValueState::Ok { span: item_span },
                                        };
                                        let result = single.from_tagged_enum(col);
                                        match result.value {
                                            Some(e) => results.push(e),
                                            None => has_errors = true,
                                        }
                                    }
                                    _ => {
                                        col.errors.borrow_mut().push(AnnotatedError::placed(
                                            item_span,
                                            &format!(
                                                "Expected inline table in array - was {}",
                                                val.type_name()
                                            ),
                                            "Only inline tables are supported in tagged enum arrays",
                                        ));
                                        has_errors = true;
                                    }
                                }
                            }

                            if has_errors {
                                TomlValue {
                                    value: Some(results),
                                    required,
                                    state: TomlValueState::ValidationFailed {
                                        span: span.clone(),
                                        message: "Array of tagged enums has errors".to_string(),
                                        help: None,
                                    },
                                }
                            } else {
                                TomlValue {
                                    value: Some(results),
                                    required,
                                    state: TomlValueState::Ok { span: span.clone() },
                                }
                            }
                        }
                        _ => TomlValue {
                            value: None,
                            required,
                            state: TomlValueState::WrongType {
                                span: span.clone(),
                                expected: "table or array",
                                found: item.type_name(),
                            },
                        },
                    }
                } else {
                    TomlValue {
                        value: None,
                        required,
                        state: TomlValueState::ValidationFailed {
                            span: span.clone(),
                            message: "Cannot convert empty value to tagged enum".to_string(),
                            help: None,
                        },
                    }
                }
            }
            _ => TomlValue {
                value: None,
                required,
                state: self.state.clone(),
            },
        }
    }
}

// Implementation of FromMapTaggedEnum for internally-tagged IndexMaps
impl<E: TaggedEnumMeta> FromMapTaggedEnum<E> for TomlValue<toml_edit::Item> {
    fn from_map_tagged_enum(self, col: &TomlCollector) -> TomlValue<IndexMap<String, E>> {
        match &self.state {
            TomlValueState::Ok { span } => {
                if let Some(ref item) = self.value {
                    match item.as_table_like_plus() {
                        Some(table) => {
                            let mut map = IndexMap::new();
                            let mut has_errors = false;

                            for (key, value) in table.iter() {
                                let item_span = value.span().unwrap_or(span.clone());
                                // Each value should be a table with internal tagging
                                let single: TomlValue<toml_edit::Item> = TomlValue {
                                    value: Some(value.clone()),
                                    required: true,
                                    state: TomlValueState::Ok {
                                        span: item_span.clone(),
                                    },
                                };
                                let result: TomlValue<E> = single.from_tagged_enum(col);
                                match result.value {
                                    Some(e) => {
                                        map.insert(key.to_string(), e);
                                    }
                                    None => {
                                        // Register the error from the failed result
                                        result.register_error(&col.errors);
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
                                        message: "Map of tagged enums has errors".to_string(),
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

// Implementation of FromMapVecTaggedEnum for internally-tagged IndexMaps of Vecs
impl<E: TaggedEnumMeta> FromMapVecTaggedEnum<E> for TomlValue<toml_edit::Item> {
    fn from_map_vec_tagged_enum(self, col: &TomlCollector) -> TomlValue<IndexMap<String, Vec<E>>> {
        match &self.state {
            TomlValueState::Ok { span } => {
                if let Some(ref item) = self.value {
                    match item.as_table_like_plus() {
                        Some(table) => {
                            let mut map: IndexMap<String, Vec<E>> = IndexMap::new();
                            let mut has_errors = false;

                            for (key, value) in table.iter() {
                                let item_span = value.span().unwrap_or(span.clone());
                                // Each value should be an array of tables with internal tagging
                                match value {
                                    toml_edit::Item::ArrayOfTables(arr) => {
                                        let mut vec_results: Vec<E> = Vec::with_capacity(arr.len());
                                        for elem_table in arr.iter() {
                                            let elem_item =
                                                toml_edit::Item::Table(elem_table.clone());
                                            let elem_span =
                                                elem_table.span().unwrap_or(item_span.clone());
                                            let single: TomlValue<toml_edit::Item> = TomlValue {
                                                value: Some(elem_item),
                                                required: true,
                                                state: TomlValueState::Ok { span: elem_span },
                                            };
                                            let result: TomlValue<E> = single.from_tagged_enum(col);
                                            match result.value {
                                                Some(e) => vec_results.push(e),
                                                None => {
                                                    result.register_error(&col.errors);
                                                    has_errors = true;
                                                }
                                            }
                                        }
                                        map.insert(key.to_string(), vec_results);
                                    }
                                    toml_edit::Item::Value(toml_edit::Value::Array(arr)) => {
                                        let mut vec_results: Vec<E> = Vec::with_capacity(arr.len());
                                        for val in arr.iter() {
                                            let val_span = val.span().unwrap_or(item_span.clone());
                                            let elem_item = toml_edit::Item::Value(val.clone());
                                            let single: TomlValue<toml_edit::Item> = TomlValue {
                                                value: Some(elem_item),
                                                required: true,
                                                state: TomlValueState::Ok { span: val_span },
                                            };
                                            let result: TomlValue<E> = single.from_tagged_enum(col);
                                            match result.value {
                                                Some(e) => vec_results.push(e),
                                                None => {
                                                    result.register_error(&col.errors);
                                                    has_errors = true;
                                                }
                                            }
                                        }
                                        map.insert(key.to_string(), vec_results);
                                    }
                                    _ => {
                                        col.errors.borrow_mut().push(AnnotatedError::placed(
                                            item_span,
                                            "Expected array for map value",
                                            "Each value in the map should be an array of tables",
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
                                        message: "Map of Vec of tagged enums has errors"
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

// Implementation of FromOptMapTaggedEnum for optional internally-tagged IndexMaps
impl<E: TaggedEnumMeta> FromOptMapTaggedEnum<E> for TomlValue<Option<toml_edit::Item>> {
    fn from_opt_map_tagged_enum(
        self,
        col: &TomlCollector,
    ) -> TomlValue<Option<IndexMap<String, E>>> {
        match &self.state {
            TomlValueState::Ok { span } => {
                if let Some(Some(ref item)) = self.value {
                    let inner: TomlValue<toml_edit::Item> = TomlValue {
                        value: Some(item.clone()),
                        required: false,
                        state: TomlValueState::Ok { span: span.clone() },
                    };
                    let result = inner.from_map_tagged_enum(col);
                    TomlValue {
                        value: result.value.map(Some),
                        required: false,
                        state: result.state,
                    }
                } else {
                    // None is valid for optional
                    TomlValue {
                        value: Some(None),
                        required: false,
                        state: TomlValueState::Ok { span: span.clone() },
                    }
                }
            }
            TomlValueState::Missing { .. } => TomlValue {
                value: Some(None),
                required: false,
                state: TomlValueState::Ok { span: 0..0 },
            },
            _ => TomlValue {
                value: None,
                required: false,
                state: self.state.clone(),
            },
        }
    }
}

// Implementation of FromOptMapVecTaggedEnum for optional internally-tagged IndexMaps of Vecs
impl<E: TaggedEnumMeta> FromOptMapVecTaggedEnum<E> for TomlValue<Option<toml_edit::Item>> {
    fn from_opt_map_vec_tagged_enum(
        self,
        col: &TomlCollector,
    ) -> TomlValue<Option<IndexMap<String, Vec<E>>>> {
        match &self.state {
            TomlValueState::Ok { span } => {
                if let Some(Some(ref item)) = self.value {
                    let inner: TomlValue<toml_edit::Item> = TomlValue {
                        value: Some(item.clone()),
                        required: false,
                        state: TomlValueState::Ok { span: span.clone() },
                    };
                    let result = inner.from_map_vec_tagged_enum(col);
                    TomlValue {
                        value: result.value.map(Some),
                        required: false,
                        state: result.state,
                    }
                } else {
                    // None is valid for optional
                    TomlValue {
                        value: Some(None),
                        required: false,
                        state: TomlValueState::Ok { span: span.clone() },
                    }
                }
            }
            TomlValueState::Missing { .. } => TomlValue {
                value: Some(None),
                required: false,
                state: TomlValueState::Ok { span: 0..0 },
            },
            _ => TomlValue {
                value: None,
                required: false,
                state: self.state.clone(),
            },
        }
    }
}
//
// pub trait AsNested<P>: Sized {
//     fn as_nested(
//         self,
//         errors: &Rc<RefCell<Vec<AnnotatedError>>>,
//         mode: FieldMatchMode,
//     ) -> TomlValue<P>;
// }
//
// impl<P> AsNested<P> for TomlValue<toml_edit::Item>
// where
//     P: FromTomlTable + VerifyFromToml<()>,
// {
//     fn as_nested(
//         self,
//         errors: &Rc<RefCell<Vec<AnnotatedError>>>,
//         mode: FieldMatchMode,
//     ) -> TomlValue<P> {
//         match &self.state {
//             TomlValueState::Ok { span } => {
//                 if let Some(ref item) = self.value {
//                     deserialize_nested(item, span, errors, mode, self.required, &[])
//                 } else {
//                     TomlValue {
//                         value: None,
//                         required: self.required,
//                         state: TomlValueState::ValidationFailed {
//                             span: span.clone(),
//                             message: "Cannot convert empty value to nested struct".to_string(),
//                             help: None,
//                         },
//                     }
//                 }
//             }
//             _ => TomlValue {
//                 value: None,
//                 required: self.required,
//                 state: self.state.clone(),
//             },
//         }
//     }
// }
//
// impl<P> AsNested<Option<P>> for TomlValue<Option<toml_edit::Item>>
// where
//     P: FromTomlTable + VerifyFromToml<()>,
// {
//     fn as_nested(
//         self,
//         errors: &Rc<RefCell<Vec<AnnotatedError>>>,
//         mode: FieldMatchMode,
//     ) -> TomlValue<Option<P>> {
//         match &self.state {
//             TomlValueState::Ok { span } => match self.value {
//                 Some(Some(item)) => {
//                     let nested = deserialize_nested(&item, span, errors, mode, self.required, &[]);
//                     match nested.value {
//                         Some(partial) => TomlValue {
//                             value: Some(Some(partial)),
//                             required: self.required,
//                             state: nested.state,
//                         },
//                         None => TomlValue {
//                             value: None,
//                             required: self.required,
//                             state: nested.state,
//                         },
//                     }
//                 }
//                 Some(None) => TomlValue {
//                     value: Some(None),
//                     required: self.required,
//                     state: TomlValueState::Ok { span: span.clone() },
//                 },
//                 None => TomlValue {
//                     value: None,
//                     required: self.required,
//                     state: TomlValueState::ValidationFailed {
//                         span: span.clone(),
//                         message: "Cannot convert empty value to nested struct".to_string(),
//                         help: None,
//                     },
//                 },
//             },
//             _ => TomlValue {
//                 value: None,
//                 required: self.required,
//                 state: self.state.clone(),
//             },
//         }
//     }
// }
//
// impl<P> AsNested<Vec<P>> for TomlValue<Vec<toml_edit::Item>>
// where
//     P: FromTomlTable + VerifyFromToml<()>,
// {
//     fn as_nested(
//         self,
//         errors: &Rc<RefCell<Vec<AnnotatedError>>>,
//         mode: FieldMatchMode,
//     ) -> TomlValue<Vec<P>> {
//         match &self.state {
//             TomlValueState::Ok { span } => match self.value {
//                 Some(items) => {
//                     let mut results: Vec<P> = Vec::with_capacity(items.len());
//
//                     for item in &items {
//                         match item {
//                             toml_edit::Item::Table(_)
//                             | toml_edit::Item::Value(toml_edit::Value::InlineTable(_)) => {
//                                 let nested = deserialize_nested(
//                                     item,
//                                     span,
//                                     errors,
//                                     mode,
//                                     self.required,
//                                     &[],
//                                 );
//                                 if let Some(partial) = nested.value {
//                                     results.push(partial);
//                                 } else {
//                                     errors.borrow_mut().push(AnnotatedError::placed(
//                                         span.clone(),
//                                         &format!(
//                                             "Expected table in array - was {}",
//                                             item.type_name()
//                                         ),
//                                         "Only table items are supported in nested struct arrays",
//                                     ));
//                                 }
//                             }
//                             _ => {
//                                 errors.borrow_mut().push(AnnotatedError::placed(
//                                     span.clone(),
//                                     &format!("Expected table in array - was {}", item.type_name()),
//                                     "Only table items are supported in nested struct arrays",
//                                 ));
//                             }
//                         }
//                     }
//
//                     if results.iter().any(|partial| !partial.can_concrete()) {
//                         TomlValue {
//                             value: Some(results),
//                             required: self.required,
//                             state: TomlValueState::ValidationFailed {
//                                 span: span.clone(),
//                                 message: "Array of nested structs has errors".to_string(),
//                                 help: None,
//                             },
//                         }
//                     } else {
//                         TomlValue {
//                             value: Some(results),
//                             required: self.required,
//                             state: TomlValueState::Ok { span: span.clone() },
//                         }
//                     }
//                 }
//                 None => TomlValue {
//                     value: None,
//                     required: self.required,
//                     state: TomlValueState::ValidationFailed {
//                         span: span.clone(),
//                         message: "Cannot convert empty value to array of nested structs"
//                             .to_string(),
//                         help: None,
//                     },
//                 },
//             },
//             _ => TomlValue {
//                 value: None,
//                 required: self.required,
//                 state: self.state.clone(),
//             },
//         }
//     }
// }

// ================================
// IndexMap / Map Support
// ================================

/// Trait for converting a TOML table into a IndexMap<String, T> where T is a primitive type
pub trait AsMap<T>: Sized {
    fn as_map(self, col: &TomlCollector) -> TomlValue<IndexMap<String, T>>;
}

/// Blanket implementation for any T that implements FromTomlItem
impl<T: FromTomlItem> AsMap<T> for TomlValue<toml_edit::Item> {
    fn as_map(self, col: &TomlCollector) -> TomlValue<IndexMap<String, T>> {
        match &self.state {
            TomlValueState::Ok { span } => {
                if let Some(ref item) = self.value {
                    match item.as_table_like_plus() {
                        Some(table) => {
                            let mut map = IndexMap::new();
                            let mut has_errors = false;

                            for (key, value) in table.iter() {
                                let item_span = value.span().unwrap_or(span.clone());
                                let val: TomlValue<T> =
                                    FromTomlItem::from_toml_item(value, item_span.clone(), col);
                                match val.state {
                                    TomlValueState::Ok { .. } => {
                                        if let Some(v) = val.value {
                                            map.insert(key.to_string(), v);
                                        }
                                    }
                                    _ => {
                                        val.register_error(&col.errors);
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

/// Blanket implementation for Option<T> where T: FromTomlItem
impl<T: FromTomlItem> AsMap<Option<T>> for TomlValue<Option<toml_edit::Item>> {
    fn as_map(self, col: &TomlCollector) -> TomlValue<IndexMap<String, Option<T>>> {
        match &self.state {
            TomlValueState::Ok { span } => match self.value {
                Some(Some(item)) => {
                    let single: TomlValue<toml_edit::Item> = TomlValue {
                        value: Some(item),
                        required: false,
                        state: TomlValueState::Ok { span: span.clone() },
                    };
                    let result: TomlValue<IndexMap<String, T>> = single.as_map(col);
                    match result.state {
                        TomlValueState::Ok { span } => {
                            let converted: IndexMap<String, Option<T>> = result
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

/// Trait for converting a TOML table into a IndexMap<String, P> where P is a nested struct
pub trait AsMapNested<P, T>: Sized {
    fn as_map_nested(self, col: &TomlCollector) -> TomlValue<IndexMap<String, P>>;
}

impl<P, T> AsMapNested<P, T> for TomlValue<toml_edit::Item>
where
    P: FromTomlTable<T> + VerifyFromToml<()>,
{
    fn as_map_nested(self, col: &TomlCollector) -> TomlValue<IndexMap<String, P>> {
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
                                    deserialize_nested(value, &item_span, col, true, &[]);
                                match nested.state {
                                    TomlValueState::Ok { .. } => {
                                        if let Some(v) = nested.value {
                                            map.insert(key.to_string(), v);
                                        }
                                    }
                                    _ => {
                                        if let Some(value) = nested.value.as_ref() {
                                            value.collect_errors(&col.errors);
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

/// Trait for converting a TOML table into a IndexMap<String, Vec<T>>
pub trait AsMapVec<T>: Sized {
    fn as_map_vec(self, col: &TomlCollector) -> TomlValue<IndexMap<String, Vec<T>>>;
}

/// Blanket implementation for AsMapVec<T> where T: FromTomlItem
impl<T: FromTomlItem> AsMapVec<T> for TomlValue<toml_edit::Item> {
    fn as_map_vec(self, col: &TomlCollector) -> TomlValue<IndexMap<String, Vec<T>>> {
        match &self.state {
            TomlValueState::Ok { span } => {
                if let Some(ref item) = self.value {
                    match item.as_table_like_plus() {
                        Some(table) => {
                            let mut map = IndexMap::new();
                            let mut has_errors = false;

                            for (key, value) in table.iter() {
                                let item_span = value.span().unwrap_or(span.clone());
                                let val: TomlValue<Vec<T>> =
                                    FromTomlItem::from_toml_item(value, item_span.clone(), col);
                                match val.state {
                                    TomlValueState::Ok { .. } => {
                                        if let Some(v) = val.value {
                                            map.insert(key.to_string(), v);
                                        }
                                    }
                                    _ => {
                                        val.register_error(&col.errors);
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
                                        message: "Map contains invalid vec values".to_string(),
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

/// Trait for converting a TOML table into IndexMap<String, Vec<T>> where each value
/// can be either a single T (wrapped in a Vec) or an array [T, ...].
pub trait AsMapVecAllowSingle<T>: Sized {
    fn as_map_vec_allow_single(self, col: &TomlCollector) -> TomlValue<IndexMap<String, Vec<T>>>;
}

/// Blanket implementation for AsMapVecAllowSingle<T> where T: FromTomlItem
impl<T: FromTomlItem> AsMapVecAllowSingle<T> for TomlValue<toml_edit::Item> {
    fn as_map_vec_allow_single(self, col: &TomlCollector) -> TomlValue<IndexMap<String, Vec<T>>> {
        match &self.state {
            TomlValueState::Ok { span } => {
                if let Some(ref item) = self.value {
                    match item.as_table_like_plus() {
                        Some(table) => {
                            let mut map = IndexMap::new();
                            let mut has_errors = false;

                            for (key, value) in table.iter() {
                                let item_span = value.span().unwrap_or(span.clone());

                                // Check if the value is an array or a single value
                                let val: TomlValue<Vec<T>> = match value {
                                    toml_edit::Item::Value(toml_edit::Value::Array(_)) => {
                                        // It's an array, parse normally as Vec<T>
                                        FromTomlItem::from_toml_item(value, item_span.clone(), col)
                                    }
                                    toml_edit::Item::ArrayOfTables(_) => {
                                        // It's an array of tables, parse normally as Vec<T>
                                        FromTomlItem::from_toml_item(value, item_span.clone(), col)
                                    }
                                    _ => {
                                        // It's a single value, try to parse as T and wrap in Vec
                                        let single: TomlValue<T> = FromTomlItem::from_toml_item(
                                            value,
                                            item_span.clone(),
                                            col,
                                        );
                                        match single.state {
                                            TomlValueState::Ok { span: s } => TomlValue {
                                                required: true,
                                                value: single.value.map(|v| vec![v]),
                                                state: TomlValueState::Ok { span: s },
                                            },
                                            other => TomlValue {
                                                required: true,
                                                value: None,
                                                state: other,
                                            },
                                        }
                                    }
                                };

                                match val.state {
                                    TomlValueState::Ok { .. } => {
                                        if let Some(v) = val.value {
                                            map.insert(key.to_string(), v);
                                        }
                                    }
                                    _ => {
                                        val.register_error(&col.errors);
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
                                        message: "Map contains invalid vec values".to_string(),
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

/// Trait for converting a TOML table into a IndexMap<String, Vec<P>> where P is a nested struct
pub trait AsMapVecNested<P, T>: Sized {
    fn as_map_vec_nested(self, col: &TomlCollector) -> TomlValue<IndexMap<String, Vec<P>>>;
}

impl<P, T> AsMapVecNested<P, T> for TomlValue<toml_edit::Item>
where
    P: FromTomlTable<T> + VerifyFromToml<()>,
{
    fn as_map_vec_nested(self, col: &TomlCollector) -> TomlValue<IndexMap<String, Vec<P>>> {
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
                                                col,
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
                                                        .collect_errors(&col.errors);
                                                    has_errors = true;
                                                }
                                            }
                                        }
                                        map.insert(key.to_string(), vec);
                                    }
                                    _ => {
                                        col.errors.borrow_mut().push(AnnotatedError::placed(
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
    fn as_opt_map(self, col: &TomlCollector) -> TomlValue<Option<IndexMap<String, T>>>;
}

/// Trait for converting an optional TOML table into Option<IndexMap<String, P>> for nested structs
pub trait AsOptMapNested<P, T>: Sized {
    fn as_opt_map_nested(self, col: &TomlCollector) -> TomlValue<Option<IndexMap<String, P>>>;
}

/// Trait for converting an optional TOML table into Option<IndexMap<String, Vec<T>>>
pub trait AsOptMapVec<T>: Sized {
    fn as_opt_map_vec(self, col: &TomlCollector) -> TomlValue<Option<IndexMap<String, Vec<T>>>>;
}

/// Trait for converting an optional TOML table into Option<IndexMap<String, Vec<P>>> for nested structs
pub trait AsOptMapVecNested<P>: Sized {
    fn as_opt_map_vec_nested(
        self,
        col: &TomlCollector,
    ) -> TomlValue<Option<IndexMap<String, Vec<P>>>>;
}

/// Blanket implementation for AsOptMap on Option<Item> returning Option<IndexMap<String, T>>
impl<T: FromTomlItem> AsOptMap<T> for TomlValue<Option<toml_edit::Item>> {
    fn as_opt_map(self, col: &TomlCollector) -> TomlValue<Option<IndexMap<String, T>>> {
        match &self.state {
            TomlValueState::Ok { span } => match self.value {
                Some(Some(item)) => {
                    let single: TomlValue<toml_edit::Item> = TomlValue {
                        value: Some(item),
                        required: false,
                        state: TomlValueState::Ok { span: span.clone() },
                    };
                    let result: TomlValue<IndexMap<String, T>> = single.as_map(col);
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

/// Implementation for AsOptMapNested on Option<Item> returning Option<IndexMap<String, P>>
impl<P, T> AsOptMapNested<P, T> for TomlValue<Option<toml_edit::Item>>
where
    P: FromTomlTable<T> + VerifyFromToml<()>,
{
    fn as_opt_map_nested(self, col: &TomlCollector) -> TomlValue<Option<IndexMap<String, P>>> {
        match &self.state {
            TomlValueState::Ok { span } => match self.value {
                Some(Some(item)) => {
                    let single: TomlValue<toml_edit::Item> = TomlValue {
                        value: Some(item),
                        required: false,
                        state: TomlValueState::Ok { span: span.clone() },
                    };
                    let result: TomlValue<IndexMap<String, P>> = single.as_map_nested(col);
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

/// Blanket implementation for AsOptMapVec<T> where T: FromTomlItem
impl<T: FromTomlItem> AsOptMapVec<T> for TomlValue<Option<toml_edit::Item>> {
    fn as_opt_map_vec(self, col: &TomlCollector) -> TomlValue<Option<IndexMap<String, Vec<T>>>> {
        match &self.state {
            TomlValueState::Ok { span } => match self.value {
                Some(Some(item)) => {
                    let single: TomlValue<toml_edit::Item> = TomlValue {
                        value: Some(item),
                        required: false,
                        state: TomlValueState::Ok { span: span.clone() },
                    };
                    let result: TomlValue<IndexMap<String, Vec<T>>> = single.as_map_vec(col);
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
