use std::{cell::RefCell, ops::Range, rc::Rc};

use indexmap::{IndexMap, IndexSet};

use crate::MapAndKeys;
use crate::case::{FieldMatchMode, suggest_alternatives};
use crate::collector::{TomlCollector, TomlSettings};
use crate::error::HydratedAnnotatedError;
use crate::tablelike::{AsTableLikePlus, TableLikePlus};
use crate::traits::Visitor;
use crate::value::{TomlValue, TomlValueState};

/// Stores information about expected fields and their aliases.
/// Used by `TomlHelper` to allow checking for unknown fields.
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

/// TOML 'table-like' access wrapper that e.g. verifies that no unexpected fields occur in your TOML
#[derive(Debug)]
pub struct TomlHelper<'a> {
    pub item: &'a toml_edit::Item,
    /// Expected field info (what we allow to see)
    expected: Vec<FieldInfo>,
    /// Normalized names that were actually observed (matched against table keys)
    observed: Vec<String>,
    /// Original field names that were allowed to the keys observed
    pub col: TomlSettings,
}

impl<'a> TomlHelper<'a> {
    /// Create a `TomlHelper` from a `toml_edit::Item` (either Table or `InlineTable`)
    #[must_use]
    pub fn from_item(item: &'a toml_edit::Item, col: TomlSettings) -> Self {
        Self {
            item,
            expected: vec![],
            observed: vec![],
            col,
        }
    }

    #[must_use]
    pub fn span(&self) -> Range<usize> {
        match self.item {
            toml_edit::Item::Table(table) => {
                let table_heading = table.span().unwrap_or(0..0);
                if table_heading.start != 0 {
                    //now find the last item is the table...
                    let mut last_end = table_heading.end;
                    for item in table.iter() {
                        if let Some(item_span) = item.1.span() {
                            if item_span.end > last_end {
                                last_end = item_span.end;
                            }
                        }
                    }
                    return last_end..last_end;
                } else {
                    table_heading // if it's the whole file (or not definied), report just at the
                    // start. Otherwise missing keys look like they belong at the end..
                }
            }
            _ => self.item.span().unwrap_or(0..0),
        }
    }

    pub fn into_inner(
        self,
        source: &Rc<RefCell<String>>,
        col: &TomlCollector,
    ) -> Vec<HydratedAnnotatedError> {
        col.errors
            .borrow_mut()
            .drain(..)
            .map(|error| HydratedAnnotatedError {
                source: source.borrow().clone(),
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
        let parent_span = self.span();

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
                    },
                    span: parent_span,
                    help: None,
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
                let spans: Vec<_> = found_keys
                    .iter()
                    .map(|(matched_key, _item)| self.span_from_key(matched_key))
                    .collect();
                for (matched_key, _) in &found_keys {
                    self.observed
                        .push(self.col.match_mode.normalize(matched_key));
                }
                let primary_span = spans[0].clone();
                TomlValue {
                    value: None,
                    state: TomlValueState::MultiDefined {
                        key: query_key.to_string(),
                        spans,
                    },
                    span: primary_span,
                    help: None,
                }
            }
        }
    }

    /// Like `get_with_aliases`, but applies `f` to each element of a `Vec` field.
    /// The adapter sees `TomlValue<S>` (the element type), not `TomlValue<Vec<S>>`.
    /// Used by the `#[tpd(with)]` macro on `Vec<T>` fields.
    pub fn get_vec_with_adapter<S, T, F>(
        &mut self,
        query_key: &str,
        aliases: &'static [&'static str],
        f: F,
    ) -> TomlValue<Vec<TomlValue<T>>>
    where
        Vec<TomlValue<S>>: Visitor + std::fmt::Debug,
        F: Fn(TomlValue<S>) -> TomlValue<T>,
    {
        self.get_with_aliases::<Vec<TomlValue<S>>>(query_key, aliases)
            .map(|v| v.into_iter().map(f).collect())
    }

    /// Like `get_with_aliases`, but applies `f` to each value of an `IndexMap` field.
    /// The adapter sees `TomlValue<S>` (the value type), not the whole map.
    /// Used by the `#[tpd(with)]` macro on `IndexMap<K, V>` fields.
    pub fn get_map_with_adapter<K, S, T, F>(
        &mut self,
        query_key: &str,
        aliases: &'static [&'static str],
        f: F,
    ) -> TomlValue<MapAndKeys<K, T>>
    where
        MapAndKeys<K, S>: Visitor + std::fmt::Debug,
        K: std::hash::Hash + Eq,
        F: Fn(TomlValue<S>) -> TomlValue<T>,
    {
        self.get_with_aliases::<MapAndKeys<K, S>>(query_key, aliases)
            .map(|m| m.map_values(f))
    }

    fn span_from_key(&self, key: &str) -> Range<usize> {
        if let Some(table) = self.item.as_table_like_plus() {
            TableLikePlus::key(table, key)
                .and_then(toml_edit::Key::span)
                .unwrap_or(0..0)
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
    pub fn absorb_remaining<K, T>(&mut self) -> TomlValue<MapAndKeys<K, T>>
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
                return TomlValue::new_ok(
                    MapAndKeys {
                        map: IndexMap::new(),
                        keys: vec![],
                    },
                    0..0,
                );
            }
        };

        let mut result_map: IndexMap<K, TomlValue<T>> = IndexMap::new();
        let mut result_keys = Vec::new();
        let mut first_span: Option<Range<usize>> = None;
        let mut last_span: Option<Range<usize>> = None;

        for (key, item) in table.iter() {
            let key_span = table
                .key(key)
                .and_then(toml_edit::Key::span)
                .unwrap_or(0..0);
            result_keys.push(TomlValue::new_ok(key.to_string(), key_span));
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
            last_span = Some(item_span);
            // Deserialize the value
            let mut helper = TomlHelper::from_item(item, self.col.clone());
            let value_result = T::fill_from_toml(&mut helper);

            result_map.insert(K::from(key_str), value_result);
        }

        let span = (first_span.unwrap_or(0..0).start)..(last_span.unwrap_or(0..0).end);

        TomlValue::new_ok(
            MapAndKeys {
                map: result_map,
                keys: result_keys,
            },
            span,
        )
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
    pub fn unknown_spans(&self) -> Vec<crate::value::UnknownKey> {
        if let Some(table) = self.item.as_table_like_plus() {
            // Collect all keys from either table type
            let keys: Vec<String> = table.iter().map(|(k, _)| k.to_string()).collect();

            // Build set of observed normalized names
            let observed_set: IndexSet<String> = self.observed.iter().cloned().collect();
            let mut alias_set: IndexSet<String> = IndexSet::new();

            for field_info in &self.expected {
                let norm_names = field_info.all_normalized_names(self.col.match_mode);
                if norm_names.iter().any(|n| observed_set.contains(n)) {
                    alias_set.extend(norm_names);
                }
            }

            let mut res = Vec::new();
            for key in keys {
                let normalized_key = self.col.match_mode.normalize(&key);

                // Check if this key was observed (i.e., it matched an expected field)
                if !observed_set.contains(&normalized_key) {
                    // This is an unknown key - find available (expected but not yet observed) fields,
                    // showing aliases as 'alias' (='canonical')
                    let still_available: Vec<String> = self
                        .expected
                        .iter()
                        .filter(|field_info| {
                            let norm_names = field_info.all_normalized_names(self.col.match_mode);
                            !norm_names.iter().any(|n| alias_set.contains(n))
                        })
                        .flat_map(|field_info| {
                            let canonical = format!("'{}'", field_info.name);
                            let alias_entries = field_info
                                .aliases
                                .iter()
                                .map(|alias| format!("'{alias}' (='{}')", field_info.name));
                            std::iter::once(canonical).chain(alias_entries)
                        })
                        .collect();

                    let span = table
                        .key(&key)
                        .and_then(toml_edit::Key::span)
                        .unwrap_or(0..0);
                    res.push(crate::value::UnknownKey {
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
