use indexmap::{IndexMap, IndexSet};
use std::fmt::Write;
use std::{cell::RefCell, ops::Range, rc::Rc};
use toml_edit::TomlError;

mod from_item;
/// Import `toml_pretty_deser::prelude::`* to make use of our macros
pub mod prelude;
mod tablelike;
pub use from_item::FromTomlItem;
pub use tablelike::{AsTableLikePlus, TableLikePlus};
mod case;

pub use case::{FieldMatchMode, suggest_alternatives};

/// The failure states of deserialization
///
/// Use `DeserError.pretty(source_name)` to get a nice error message with code snippets and hints.
#[derive(Debug)]
pub enum DeserError<P> {
    /// TOML parsing failed. No `PartialT` available.
    /// A wrapper around [`toml_edit::TomlError`]
    ParsingFailure(TomlError, String),
    /// Parsing suceeded, but deserialization failed. `PartialT` available with whatever could be
    /// understood.
    DeserFailure(Vec<HydratedAnnotatedError>, P),
}

impl<P> DeserError<P> {
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
            Self::DeserFailure(items, _) => {
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
}

/// The non-pretty representation of deser errors.
#[derive(Debug, Clone)]
pub struct AnnotatedError {
    // spans on the TOML source and the messages we should display for them.
    pub spans: Vec<SpannedMessage>,
    /// A helpful hint that's not show inline with the code
    pub help: Option<String>,
}

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
    pub source: Rc<RefCell<String>>,
    pub inner: AnnotatedError,
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
}

/// Convenience methods for building up `AnnotatedErrors`
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

fn pretty_error_message(
    source_name: &str,
    source: &str,
    spans: &[SpannedMessage],
    help: Option<&String>,
) -> String {
    use bstr::{BStr, ByteSlice};
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

        let previous_newline =
            memchr::memmem::rfind(&source.as_bytes()[..spans[0].span.start], b"\n");
        let this_line_is_block_start = source.as_bytes()[previous_newline.unwrap_or(0)..]
            .trim_ascii_start()
            .starts_with(b"[");

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
            .map(|x| x.trim_end())
            .fold(String::new(), |acc, line| acc + line + "\n");
        let digits_needed = blockf
            .chars()
            .position(|c| c == '│')
            .map(|x| x - 1)
            .unwrap_or(1);

        let lines_before = match previous_newline {
            None => String::new(),
            Some(previous_newline) => {
                let lines: Vec<_> = {
                    let upto_span = &BStr::new(source.as_bytes())[..previous_newline];
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
            write!(&mut out, "\n").expect("can't fail");
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

impl HydratedAnnotatedError {
    #[must_use]
    pub fn pretty(&self, source_name: &str) -> String {
        pretty_error_message(
            source_name,
            self.source.borrow().as_str(),
            &self.inner.spans,
            self.inner.help.as_ref(),
        )
    }
}

/// Stores information about expected fields and their aliases
#[doc(hidden)]
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

    #[must_use]
    pub fn with_alias(mut self, alias: &'static str) -> Self {
        self.aliases.push(alias);
        self
    }

    #[must_use]
    pub fn with_aliases(mut self, aliases: &'static [&'static str]) -> Self {
        self.aliases.extend(aliases);
        self
    }

    /// Get all normalized names for this field (primary name + aliases)
    #[must_use]
    pub fn all_normalized_names(&self, mode: &FieldMatchMode) -> Vec<String> {
        let mut names = vec![mode.normalize(&self.name)];
        for alias in &self.aliases {
            names.push(mode.normalize(alias));
        }
        names
    }
}

/// Parameter to [`deserialize_with_mode`]
#[derive(Clone, Debug)]
pub enum VecMode {
    /// Accept single values in lieu of ```[value]```
    SingleOk,
    /// Vecs must be TOML arrays. Always.
    Strict,
}

/// Grab bag that collects the errors and provides parameterisation to
/// the deser functions
#[derive(Clone, Debug)]
pub struct TomlCollector {
    pub errors: Rc<RefCell<Vec<AnnotatedError>>>,
    pub match_mode: FieldMatchMode,
    pub vec_mode: VecMode,
    /// Context spans that will be added to errors (e.g., "Involving this variant")
    /// These are stored as (span, message) pairs and are added to all errors registered
    /// while the context is active.
    pub context_spans: Rc<RefCell<Vec<SpannedMessage>>>,
}

impl TomlCollector {
    /// Add a context span that will be appended to all errors registered while this context is active.
    /// Returns the number of context spans before this one was added (for use with `pop_context_to`).
    pub fn push_context(&self, span: Range<usize>, msg: &str) -> usize {
        let mut contexts = self.context_spans.borrow_mut();
        let count = contexts.len();
        contexts.push(SpannedMessage {
            span,
            msg: msg.to_string(),
        });
        count
    }

    /// Remove context spans back to a specific count (returned by `push_context`).
    pub fn pop_context_to(&self, count: usize) {
        let mut contexts = self.context_spans.borrow_mut();
        contexts.truncate(count);
    }

    /// Get a copy of the current context spans.
    pub fn get_context_spans(&self) -> Vec<SpannedMessage> {
        self.context_spans.borrow().clone()
    }
}

/// TOML 'table-like' access wrapper that e.g. verifies that no unexpected fields occur in your TOML
pub struct TomlHelper<'a> {
    pub table: Option<&'a dyn TableLikePlus>,
    /// Expected field info (what we allow to see)
    expected: Vec<FieldInfo>,
    /// Normalized names that were actually observed (matched against table keys)
    observed: Vec<String>,
    /// Original field names that were allowed to the keys observed
    pub col: TomlCollector,
}

impl<'a> TomlHelper<'a> {
    pub fn from_table(table: &'a dyn TableLikePlus, col: TomlCollector) -> Self {
        Self {
            table: Some(table),
            expected: vec![],
            observed: vec![],
            col,
        }
    }

    /// Create a `TomlHelper` from a `toml_edit::Item` (either Table or `InlineTable`)
    #[must_use]
    pub fn from_item(item: &'a toml_edit::Item, col: &TomlCollector) -> Self {
        match item.as_table_like_plus() {
            Some(table) => Self::from_table(table, col.clone()),
            _ => Self {
                table: None,
                expected: vec![],
                observed: vec![],
                col: col.clone(),
            },
        }
    }

    pub fn span(&self) -> Range<usize> {
        if let Some(table) = self.table {
            table.span().unwrap_or(0..0)
        } else {
            0..0
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
    ///
    // /// Register a field with optional aliases
    // pub fn ignore_field(&mut self, name: impl Into<String>) {
    //     let name: String = name.into();
    //     let field_info = FieldInfo::new(name.clone());
    //     self.expected.push(field_info);
    //     self.observed.push(name);
    // }

    /// Find a key in the table that matches the given field name (considering aliases and match mode)
    fn find_matching_keys(
        &self,
        name: &str,
        aliases: &[&'static str],
    ) -> Vec<(String, toml_edit::Item)> {
        let mut result = Vec::new();
        let _normalized_target = self.col.match_mode.normalize(name);
        let candidates = std::iter::once(name.to_string())
            .chain(aliases.iter().map(std::string::ToString::to_string))
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
                if self.col.match_mode.matches(candidate, table_key)
                    && let Some(table) = self.table
                    && let Some(item) = table.get(table_key)
                {
                    result.push((table_key.clone(), item.clone()));
                    break;
                }
            }
        }

        result
    }

    /// # Panics
    /// Shouldn't.
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
        let found_keys = self.find_matching_keys(query_key, aliases);

        match found_keys.len() {
            0 => {
                // No match found
                let res = TomlValue {
                    value: None,
                    state: TomlValueState::Missing {
                        key: query_key.to_string(),
                        parent_span,
                    },
                };
                res
            }
            1 => {
                let (matched_key, item) = found_keys.first().expect("can't fail");
                let res: TomlValue<T> = FromTomlItem::from_toml_item(item, parent_span, &self.col);
                self.observed
                    .push(self.col.match_mode.normalize(matched_key));
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

                let res = TomlValue {
                    value: None,
                    state: TomlValueState::MultiDefined {
                        key: query_key.to_string(),
                        spans,
                    },
                };
                res
            }
        }
    }

    /// Like `get_with_aliases`, but does not automatically register errors for type mismatches.
    /// This is useful when you want to try parsing a value as multiple types (e.g., either
    /// a string or a number) and handle the error yourself.
    ///
    /// # Panics
    /// Shouldn't.
    pub fn get_with_aliases_no_auto_error<T>(
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
        let found_keys = self.find_matching_keys(query_key, aliases);

        match found_keys.len() {
            0 => {
                // No match found
                let res = TomlValue {
                    value: None,
                    state: TomlValueState::Missing {
                        key: query_key.to_string(),
                        parent_span,
                    },
                };
                res
            }
            1 => {
                let (matched_key, item) = found_keys.first().expect("can't fail");
                let res: TomlValue<T> = FromTomlItem::from_toml_item(item, parent_span, &self.col);
                self.observed
                    .push(self.col.match_mode.normalize(matched_key));
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

                let res = TomlValue {
                    value: None,
                    state: TomlValueState::MultiDefined {
                        key: query_key.to_string(),
                        spans,
                    },
                };
                res
            }
        }
    }

    fn span_from_key(&self, key: &str) -> Range<usize> {
        if let Some(table) = self.table {
            table
                .key(key)
                .and_then(toml_edit::Key::span)
                .unwrap_or(0..0)
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
        let err = AnnotatedError::placed(span, msg, help);
        self.col.errors.borrow_mut().push(err);
    }

    #[allow(clippy::missing_panics_doc)]
    pub fn add_err_by_spans(&self, spans: Vec<(Range<usize>, String)>, help: &str) {
        let err = if spans.is_empty() {
            AnnotatedError::unplaced(help)
        } else {
            let mut iter = spans.into_iter();
            let first = iter.next().unwrap();
            let mut err = AnnotatedError::placed(first.0, &first.1, help);
            for (span, msg) in iter {
                err.add_span(span, &msg);
            }
            err
        };
        self.col.errors.borrow_mut().push(err);
    }

    /// Absorb all remaining keys (not matched by other fields) into an `IndexMap`.
    /// This is used by `#[tpd_absorb_remaining]` fields.
    ///
    /// The function iterates over all keys in the table, skips those that were
    /// already matched (in `self.observed`), and deserializes the remaining keys
    /// into an `IndexMap<K, T>` where K implements `From<String>`.
    ///
    /// The absorbed keys are marked as observed, so `deny_unknown()` won't report them.
    ///
    /// Returns a `TomlValue` containing the map. The map preserves insertion order.
    pub fn absorb_remaining<K, T>(&mut self) -> TomlValue<IndexMap<K, TomlValue<T>>>
    where
        K: From<String> + std::hash::Hash + Eq,
        T: FromTomlItem,
    {
        // Build set of observed normalized names (keys that matched other fields)
        let observed_set: IndexSet<String> = self.observed.iter().cloned().collect();

        // Collect all keys from the table
        let table = match self.table {
            Some(t) => t,
            None => {
                // No table - return empty map
                return TomlValue::new_ok(IndexMap::new(), 0..0);
            }
        };

        let mut result_map: IndexMap<K, TomlValue<T>> = IndexMap::new();
        let mut all_ok = true;
        let mut first_span: Option<Range<usize>> = None;

        for (key, item) in table.iter() {
            let key_str = key.to_string();
            let normalized_key = self.col.match_mode.normalize(&key_str);

            // Skip keys that were already matched by other fields
            if observed_set.contains(&normalized_key) {
                continue;
            }

            // Mark this key as observed (absorbed) so deny_unknown won't report it
            self.observed.push(normalized_key);

            // Get span for this item
            let item_span = item.span().unwrap_or(0..0);
            if first_span.is_none() {
                first_span = Some(item_span.clone());
            }
            // Deserialize the value
            let value_result = T::from_toml_item(item, item_span, &self.col);

            if !value_result.is_ok() {
                all_ok = false;
            }
            result_map.insert(K::from(key_str), value_result);
        }

        let span = first_span.unwrap_or(0..0);

        if all_ok {
            TomlValue::new_ok(result_map, span)
        } else {
            // Return the partial map but indicate nested errors
            TomlValue {
                value: Some(result_map),
                state: TomlValueState::Nested {},
            }
        }
    }

    pub fn no_unknown(&self) -> bool {
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
                return false;
            }
        }
        return true;
    }

    pub fn register_unknown(&mut self) {
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
                    .map(std::string::String::as_str)
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

/// The internal representation of a value to-have-been-deserialized
#[derive(Debug)]
pub struct TomlValue<T> {
    pub value: Option<T>,
    pub state: TomlValueState,
}

impl<T> TomlValue<T> {
    pub const fn new_ok(value: T, span: Range<usize>) -> Self {
        Self {
            value: Some(value),
            state: TomlValueState::Ok { span },
        }
    }

    #[must_use]
    pub const fn new_empty_missing(parent_span: Range<usize>) -> Self {
        Self {
            value: None,
            state: TomlValueState::Missing {
                key: String::new(),
                parent_span,
            },
        }
    }
    #[must_use]
    pub const fn new_validation_failed(
        span: Range<usize>,
        message: String,
        help: Option<String>,
    ) -> Self {
        Self {
            value: None,
            state: TomlValueState::ValidationFailed {
                span,
                message,
                help,
            },
        }
    }

    #[must_use]
    pub fn new_wrong_type(
        item: &toml_edit::Item,
        parent_span: Range<usize>,
        expected: &'static str,
    ) -> Self {
        Self {
            value: None,
            state: TomlValueState::WrongType {
                span: item.span().unwrap_or(parent_span),
                expected,
                found: item.type_name(),
            },
        }
    }

    #[must_use]
    pub fn new_nested() -> Self {
        Self {
            value: None,
            state: TomlValueState::Nested {},
        }
    }

    pub fn convert_failed_type<S>(self) -> TomlValue<S> {
        match self.state {
            TomlValueState::Ok { span } => {
                panic!("called convert_failed_type on a TomlValue that is Ok. Span was: {span:?}")
            }
            _ => TomlValue {
                value: None,
                state: self.state,
            },
        }
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
                value: Some(self.value),
                state: TomlValueState::Ok { span },
            },
            TomlValueState::Missing {
                key: _,
                parent_span,
            } => TomlValue {
                value: Some(None),
                state: TomlValueState::Ok { span: parent_span },
            },
            TomlValueState::Nested{} => TomlValue {
                value: Some(self.value),
                state: TomlValueState::Nested{},
            },
            _ => TomlValue {
                value: None,
                state: self.state,
            },
        }
    }

    pub fn is_ok(&self) -> bool {
        matches!(self.state, TomlValueState::Ok { .. })
    }

    pub fn is_missing(&self) -> bool {
        matches!(self.state, TomlValueState::Missing { .. })
    }

    pub fn as_ref(&self) -> Option<&T> {
        match self.state {
            TomlValueState::Ok { .. } => self.value.as_ref(),
            _ => None,
        }
    }

    pub fn as_mut(&mut self) -> Option<&mut T> {
        match self.state {
            TomlValueState::Ok { .. } => self.value.as_mut(),
            _ => None,
        }
    }

    pub fn span(&self) -> Range<usize> {
        match &self.state {
            TomlValueState::Ok { span }
            | TomlValueState::Missing {
                parent_span: span, ..
            }
            | TomlValueState::WrongType { span, .. }
            | TomlValueState::ValidationFailed { span, .. } => span.clone(),
            TomlValueState::NotSet | TomlValueState::Nested => 0..0,
            TomlValueState::MultiDefined { spans, .. } => spans[0].clone(), //just return the first one
        }
    }

    /// # Panics
    /// When the value is not `TomlValueState::Ok`
    pub fn expect(self, msg: &str) -> T {
        match self.state {
            TomlValueState::Ok { .. } => self.value.expect(msg),
            _ => panic!("Called unwrap on a TomlValue that is not Ok"),
        }
    }

    /// Register an error using the context spans from the collector.
    pub fn register_error(&self, col: &TomlCollector) {
        let context = col.get_context_spans();
        self.register_error_with_context(&col.errors, &context);
    }

    /// Register an error with additional context spans that will be appended to the error.
    fn register_error_with_context(
        &self,
        errors: &Rc<RefCell<Vec<AnnotatedError>>>,
        context_spans: &[SpannedMessage],
    ) {
        let mut err = match &self.state {
            TomlValueState::NotSet | TomlValueState::Ok { .. } | TomlValueState::Nested => {
                return;
            } //ignored, we expect the errors below to have been added
            TomlValueState::Missing { key, parent_span } => AnnotatedError::placed(
                parent_span.clone(),
                &format!("Missing required key: '{key}'."),
                "This key is required but was not found in the TOML document.",
            ),
            TomlValueState::MultiDefined { key, spans } => {
                let mut err = AnnotatedError::placed(
                    spans[0].clone(),
                    "Key/alias conflict (defined multiple times)",
                    &format!("Use only one of the keys involved. Canonical is '{key}'"),
                );
                for span in spans.iter().skip(1) {
                    err.add_span(span.clone(), "Also defined here");
                }
                err
            }
            TomlValueState::WrongType {
                span,
                expected,
                found,
            } => AnnotatedError::placed(
                span.clone(),
                &format!("Wrong type: expected {expected}, found {found}"),
                "This value has the wrong type.",
            ),
            TomlValueState::ValidationFailed {
                span,
                message,
                help,
            } => AnnotatedError::placed(
                span.clone(),
                message,
                help.as_ref().map_or("", std::string::String::as_str),
            ),
        };

        // Add context spans to the error
        for context in context_spans {
            err.add_span(context.span.clone(), &context.msg);
        }

        errors.borrow_mut().push(err);
    }

    #[allow(clippy::missing_panics_doc)]
    #[must_use]
    pub fn verify<F>(self, _helper: &mut TomlHelper, verification_func: F) -> Self
    where
        F: FnOnce(&T) -> Result<(), (String, Option<String>)>,
    {
        match &self.state {
            TomlValueState::Ok { span } => match verification_func(
                self.value
                    .as_ref()
                    .expect("None value on TomlValueState::Ok"),
            ) {
                Ok(()) => self,
                Err((msg, help)) => {
                    Self {
                        value: None,
                        state: TomlValueState::ValidationFailed {
                            span: span.clone(),
                            message: msg,
                            help, //todo
                        },
                    }
                }
            },
            _ => self,
        }
    }

    #[must_use]
    pub fn or_default(self, default: T) -> Self {
        match &self.state {
            TomlValueState::Missing { .. } => Self {
                value: Some(default),
                state: TomlValueState::Ok { span: 0..0 },
            },
            _ => self,
        }
    }
    #[must_use]
    pub fn or_default_with<F>(self, default_func: F) -> Self
    where
        F: FnOnce() -> T,
    {
        match &self.state {
            TomlValueState::Missing { .. } => Self {
                value: Some(default_func()),
                state: TomlValueState::Ok { span: 0..0 },
            },
            _ => self,
        }
    }
}

impl<T> Default for TomlValue<T> {
    fn default() -> Self {
        TomlValue {
            value: None,
            state: TomlValueState::NotSet,
        }
    }
}

/// Captures precisely what went wrong
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
    Nested,
    Ok {
        span: Range<usize>,
    },
}
