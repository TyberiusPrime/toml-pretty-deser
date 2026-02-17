#![doc=include_str!("../README.md")]
use indexmap::{IndexMap, IndexSet};
use std::fmt::Write;
use std::{cell::RefCell, ops::Range, rc::Rc};
use toml_edit::TomlError;

/// Import `toml_pretty_deser::prelude::`* to make use of our macros
pub mod prelude;
mod tablelike;
pub use tablelike::{AsTableLikePlus, TableLikePlus};
mod case;
pub mod helpers;
mod visitors;

pub use case::{FieldMatchMode, suggest_alternatives};
pub use helpers::{Root, VerifyVisitor, Visitor};

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
    DeserFailure(Vec<HydratedAnnotatedError>, Box<P>),
}

impl<P> DeserError<P> {
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

/// The main user facing verification trait.
///
/// Implement this on your `PartialT` for the parent type
/// that it's going to see from your nested structure.
///
/// If this is a top level `Partial`, use [`Root`]
///
/// If you don't need the parent object for verification use
/// ```rust, ignore
/// use toml_pretty_deser::prelude::*;
///
/// impl <R> VerifyIn<R> for PartialT {
///
///    fn verify(&mut self,
///     helper: &mut TomlHelper<'_>,
///     parent: &R)
///     -> Result<(), (String, Option<String>)> {
///       // ...
///    }
/// }
///    ```
///
///
pub trait VerifyIn<Parent> {
    #[allow(unused_variables)]
    /// # Errors
    /// When the developer wants to replace this value with
    /// a `TomlValue` in failed verification state.
    fn verify(
        &mut self,
        helper: &mut TomlHelper<'_>,
        parent: &Parent,
    ) -> Result<(), (String, Option<String>)>
    where
        Self: Sized + Visitor,
    {
        Ok(())
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

#[allow(clippy::too_many_lines)]
fn pretty_error_message(
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
                                format!(
                                    "{:>digits_needed$} │ {line}",
                                    line_no + 1,
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
/// For deny_uknown.
#[doc(hidden)]
#[derive(Debug, Clone)]
struct FieldInfo {
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
    pub fn with_aliases(mut self, aliases: &'static [&'static str]) -> Self {
        self.aliases.extend(aliases);
        self
    }

    /// Get all normalized names for this field (primary name + aliases)
    #[must_use]
    pub fn all_normalized_names(&self, mode: FieldMatchMode) -> Vec<String> {
        let mut names = vec![mode.normalize(&self.name)];
        for alias in &self.aliases {
            names.push(mode.normalize(alias));
        }
        names
    }
}

/// Parameter to `tpd_from_toml` that controls whether
/// single values in lieu of an array are ok.
#[derive(Clone, Debug)]
pub enum VecMode {
    /// Accept single values in lieu of ```[value]```
    SingleOk,
    /// Vecs must be TOML arrays. Always.
    Strict,
}

impl VecMode {
    fn single_ok(&self) -> bool {
        matches!(self, VecMode::SingleOk)
    }
}

/// Grab bag that collects the errors and provides parameterisation to
/// the deser functions
#[doc(hidden)]
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
    #[must_use]
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
    #[must_use]
    pub fn get_context_spans(&self) -> Vec<SpannedMessage> {
        self.context_spans.borrow().clone()
    }
}

/// TOML 'table-like' access wrapper that e.g. verifies that no unexpected fields occur in your TOML
#[derive(Debug)]
pub struct TomlHelper<'a> {
    //pub table: Option<&'a dyn TableLikePlus>,
    pub item: &'a toml_edit::Item,
    /// Expected field info (what we allow to see)
    expected: Vec<FieldInfo>,
    /// Normalized names that were actually observed (matched against table keys)
    observed: Vec<String>,
    /// Original field names that were allowed to the keys observed
    pub col: TomlCollector,
}

impl<'a> TomlHelper<'a> {
    // pub fn from_table(table: &'a dyn TableLikePlus, col: TomlCollector) -> Self {
    //     Self {
    //         table: Some(table),
    //         expected: vec![],
    //         observed: vec![],
    //         col,
    //     }
    // }

    /// Create a `TomlHelper` from a `toml_edit::Item` (either Table or `InlineTable`)
    #[must_use]
    pub fn from_item(item: &'a toml_edit::Item, col: TomlCollector) -> Self {
        Self {
            item,
            expected: vec![],
            observed: vec![],
            col,
        }
    }

    #[must_use]
    pub fn span(&self) -> Range<usize> {
        self.item.span().unwrap_or(0..0)
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

    #[must_use]
    pub fn is_table(&self) -> bool {
        matches!(
            self.item,
            toml_edit::Item::Table(_) | toml_edit::Item::Value(toml_edit::Value::InlineTable(_))
        )
    }

    /// Find a key in the table that matches the given field name (considering aliases and match mode)
    fn find_matching_keys(
        &self,
        name: &str,
        aliases: &[&'static str],
    ) -> Vec<(String, toml_edit::Item)> {
        let mut result: Vec<(String, toml_edit::Item)> = Vec::new();
        let _normalized_target = self.col.match_mode.normalize(name);
        let candidates = std::iter::once(name.to_string())
            .chain(aliases.iter().map(std::string::ToString::to_string))
            .collect::<Vec<_>>();

        let table: &dyn TableLikePlus = self.item.as_table_like_plus().expect("Not a table");
        // Collect all table keys
        let table_keys: Vec<String> = {
            table
                .iter()
                .map(|(k, _): (&str, &toml_edit::Item)| k.to_string())
                .collect()
        };

        // Try to find a match
        for table_key in &table_keys {
            for candidate in &candidates {
                if self.col.match_mode.matches(candidate, table_key)
                    && let Some(item) = table.get(table_key)
                {
                    result.push((table_key.clone(), <toml_edit::Item>::clone(item))); //xxx
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
        T: Visitor + std::fmt::Debug,
    {
        let parent_span = self.item.span().unwrap_or(0..0);

        // Register this field as expected
        self.expect_field(query_key, aliases);

        // Try to find a matching key (considering aliases and match mode)
        let found_keys = self.find_matching_keys(query_key, aliases);

        match found_keys.len() {
            0 => {
                // No match found

                TomlValue {
                    value: None,
                    state: TomlValueState::Missing {
                        key: query_key.to_string(),
                        parent_span,
                    },
                }
            }
            1 => {
                let (matched_key, item) = found_keys.first().expect("can't fail");
                let mut helper = TomlHelper::from_item(item, self.col.clone());
                let res: TomlValue<T> = T::fill_from_toml(&mut helper);
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

                TomlValue {
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
        if let Some(table) = self.item.as_table_like_plus() {
            TableLikePlus::key(table, key)
                .and_then(toml_edit::Key::span)
                .unwrap_or(0..0)
        // } else if let Some(inline_table) = self.inline_table {
        //     // For inline tables, we can't easily get the key span, so use the table span
        //     inline_table.span().unwrap_or(0..0)
        } else {
            0..0
        }
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
    #[allow(clippy::manual_let_else)]
    pub fn absorb_remaining<K, T>(&mut self) -> TomlValue<IndexMap<K, TomlValue<T>>>
    where
        K: From<String> + std::hash::Hash + Eq,
        T: Visitor + Default,
    {
        // Build set of observed normalized names (keys that matched other fields)
        let observed_set: IndexSet<String> = self.observed.iter().cloned().collect();

        // Collect all keys from the table
        let table = match self.item.as_table_like_plus() {
            Some(t) => t,
            None => {
                // No table - return empty map
                return TomlValue::new_ok(IndexMap::new(), 0..0);
            }
        };

        let mut result_map: IndexMap<K, TomlValue<T>> = IndexMap::new();
        // let mut all_ok = true;
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
            let mut helper = TomlHelper::from_item(item, self.col.clone());
            let value_result = T::fill_from_toml(&mut helper);

            // if !value_result.is_ok() {
            //     all_ok = false;
            // }
            result_map.insert(K::from(key_str), value_result);
        }

        let span = first_span.unwrap_or(0..0);

        // if all_ok {
        //feels wrong, but we're calling can_concrete on it
        //which in turn asks the values, and  that does the check
        TomlValue::new_ok(result_map, span)
        // } else {
        //     // Return the partial map but indicate nested errors
        //     TomlValue {
        //         value: Some(result_map),
        //         state: TomlValueState::Nested {},
        //     }
        // }
    }

    #[must_use]
    pub fn has_unknown(&self) -> bool {
        let mut expected_normalized: IndexSet<String> = IndexSet::new();
        for field_info in &self.expected {
            for normalized_name in field_info.all_normalized_names(self.col.match_mode) {
                expected_normalized.insert(normalized_name);
            }
        }

        // Collect all keys from either table type
        let keys: Vec<String> = if let Some(table) = self.item.as_table_like_plus() {
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
                return true;
            }
        }
        false
    }

    /// Find the spans+other information of all unknown keys.
    ///
    /// # Panics
    /// when called on a non-table item
    pub fn unknown_spans(&self) -> Vec<UnknownKey> {
        // Build set of normalized expected names (including aliases)
        let mut expected_normalized: IndexSet<String> = IndexSet::new();
        for field_info in &self.expected {
            for normalized_name in field_info.all_normalized_names(self.col.match_mode) {
                expected_normalized.insert(normalized_name);
            }
        }
        if let Some(table) = self.item.as_table_like_plus() {
            // Collect all keys from either table type
            let keys: Vec<String> = table.iter().map(|(k, _)| k.to_string()).collect();

            // Build set of observed normalized names
            let observed_set: IndexSet<String> = self.observed.iter().cloned().collect();

            let mut res = Vec::new();
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

                    let span = table
                        .key(&key)
                        .and_then(toml_edit::Key::span)
                        .unwrap_or(0..0);
                    res.push(UnknownKey {
                        key: key.clone(),
                        span,
                        help: suggest_alternatives(&normalized_key, &still_available),
                        additional_spans: vec![],
                    });
                }
            }
            res
        } else {
            panic!("unknown_spans called on a non-table toml item")
        }
    }
}

/// The Result+Option representation of a TOML value that we will have
/// attempted to deserialize. Eventually.
///
/// The main component of `PartialT`
#[derive(Debug)]
pub struct TomlValue<T> {
    /// Was this value deserialized successfully,
    /// and if not, why not
    pub state: TomlValueState,
    /// Value is independent of the state,
    /// so that containers can pinpoint which of their values failed.
    pub value: Option<T>,
}

impl<T> TomlValue<T> {
    /// Create a new `TomlValue` in the Ok state with the given value and span.
    #[must_use]
    pub const fn new_ok(value: T, span: Range<usize>) -> Self {
        Self {
            value: Some(value),
            state: TomlValueState::Ok { span },
        }
    }

    /// Create a new custom `TomlValue` error with one or multiple spans
    /// for errors that don't fit the framewrok
    ///
    ///
    /// # Panics
    /// when spans is empty
    #[must_use]
    pub fn new_custom(
        value: Option<T>,
        spans: Vec<(Range<usize>, String)>,
        help: Option<&str>,
    ) -> Self {
        assert!(
            !spans.is_empty(),
            "new_custom should have at least one span"
        );
        Self {
            value,
            state: TomlValueState::Custom {
                help: help.map(ToString::to_string),
                spans,
            },
        }
    }

    /// Create a new `TomlValue` in the Missing state with the given parent span.
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
    /// Create a new `TomlValue` with a `ValidationFailed` state.
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

    /// Create a new `TomlValue` than reflects a wrong type being used in the TOML
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

    /// Create a new `TomlValue` in nested error state.
    #[must_use]
    pub fn new_nested(value: Option<T>) -> Self {
        Self {
            value,
            state: TomlValueState::Nested {},
        }
    }

    /// Convert a `TomlValue<T>` into a `TomlValue<Option<T>`,
    /// and promote Missing from an error state into `Ok`
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
            TomlValueState::Nested => TomlValue {
                value: Some(self.value),
                state: TomlValueState::Nested {},
            },
            _ => TomlValue {
                value: None,
                state: self.state,
            },
        }
    }

    /// Change the inner value if state is `TomlValueState::Ok`,
    /// otherwise adapt the error type but loose the value.
    /// # Panics
    /// When the ok -> value present invariant is violated
    pub fn map<R, F>(self, map_function: F) -> TomlValue<R>
    where
        F: FnOnce(T) -> R,
    {
        match &self.state {
            TomlValueState::Ok { span } => {
                TomlValue::new_ok(map_function(self.value.unwrap()), span.clone())
            }
            _ => self.convert_failed_type(),
        }
    }

    /// Change the inner value no matter if we're ok or not.
    pub fn map_any<R, F>(self, map_function: F) -> TomlValue<R>
    where
        F: FnOnce(T) -> R,
    {
        TomlValue {
            state: self.state,
            value: self.value.map(map_function),
        }
    }

    /// Adapt failed types, eating the value
    ///
    /// # Panics
    ///
    /// When called on an ok `TomlValue`
    pub fn convert_failed_type<S>(&self) -> TomlValue<S> {
        match &self.state {
            TomlValueState::Ok { span } => {
                panic!("called convert_failed_type on a TomlValue that is Ok. Span was: {span:?}")
            }
            _ => TomlValue {
                value: None,
                state: self.state.clone(),
            },
        }
    }

    /// take the value out of the `TomlValue`, leaving a `TomlValueState::NotSet` and None in place.
    #[must_use]
    pub fn take(&mut self) -> Self {
        std::mem::replace(
            self,
            TomlValue {
                value: None,
                state: TomlValueState::NotSet,
            },
        )
    }

    /// Is this `TomlValue` in the Ok state?
    pub fn is_ok(&self) -> bool {
        matches!(self.state, TomlValueState::Ok { .. })
    }

    /// Get a reference to the inner value iff this `TomlValue` is in the Ok state, otherwise None.
    pub fn as_ref(&self) -> Option<&T> {
        match self.state {
            TomlValueState::Ok { .. } => self.value.as_ref(),
            _ => None,
        }
    }

    /// Get a mutable reference to the inner value iff this `TomlValue` is in the Ok state, otherwise None.
    pub fn as_mut(&mut self) -> Option<&mut T> {
        match self.state {
            TomlValueState::Ok { .. } => self.value.as_mut(),
            _ => None,
        }
    }

    /// Retrieve the primary span in the input TOML source.
    /// If no such span is available, return the 0..0 span.
    ///
    ///
    /// # Panics
    ///
    /// When called on `UnknownKeys`
    /// When called on a Custom without spans
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
            TomlValueState::UnknownKeys(_) => panic!("don't call span on UnknownKeys?"),
            TomlValueState::Custom { spans, .. } => spans[0].0.clone(),
        }
    }

    /// Verify this TOML value
    ///
    /// Calls `verification_func` with a reference to the inner value
    /// if this `TomlValue` is in the Ok state,
    ///     and returns a new `TomlValue` that reflects the result of the verification:
    /// otherwise, it remains in a previous error state.
    #[allow(clippy::missing_panics_doc)]
    #[must_use]
    pub fn verify<F>(self, verification_func: F) -> Self
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

    /// Replace the value with `default`if it was `Missing`
    #[must_use]
    pub fn or(self, default: T) -> Self {
        match &self.state {
            TomlValueState::Missing { .. } => Self {
                value: Some(default),
                state: TomlValueState::Ok { span: 0..0 },
            },
            _ => self,
        }
    }

    /// Replace the value with the result of `default_func` if it was `Missing`
    #[must_use]
    pub fn or_with<F>(self, default_func: F) -> Self
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

    /// Replace the value with `T::default()` if it was `Missing`
    #[must_use]
    pub fn or_default(self) -> Self
    where
        T: Default,
    {
        self.or_with(Default::default)
    }
}

/// `TomlValues` default to `NotSet`
impl<T> Default for TomlValue<T> {
    fn default() -> Self {
        TomlValue {
            value: None,
            state: TomlValueState::NotSet,
        }
    }
}

/// Inner struct for `TomlValue::UnknownKeys`
#[derive(Debug, Clone)]
pub struct UnknownKey {
    pub key: String, // captured for user code
    pub span: Range<usize>,
    pub help: String,
    pub additional_spans: Vec<(Range<usize>, String)>,
}

/// Captures precisely what went wrong
#[derive(Debug, Clone)]
pub enum TomlValueState {
    /// This value has not been set yet
    NotSet,
    /// This value was missing - and that's a problem
    Missing {
        key: String,
        parent_span: Range<usize>,
    },
    /// This value was defined more than once
    /// possibly using aliases.
    /// Spans point to all definitions.
    MultiDefined {
        key: String,
        spans: Vec<Range<usize>>,
    },
    /// This value had the wrong type
    WrongType {
        span: Range<usize>,
        expected: &'static str,
        found: &'static str,
    },
    /// This value had the right type, but failed validation
    ValidationFailed {
        span: Range<usize>,
        message: String,
        help: Option<String>,
    },
    /// There were one-or-more unknown keys within this table.
    UnknownKeys(Vec<UnknownKey>),
    /// This is a container, and one of it's children is in an error state
    Nested,
    /// A user defined error with multiple spans
    Custom {
        help: Option<String>,
        spans: Vec<(Range<usize>, String)>,
    },
    /// This value was deserialized correctly.
    Ok { span: Range<usize> },
}
