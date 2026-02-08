//! Tests for #[tpd_absorb_remaining] attribute
//!
//! This attribute allows a field of type `IndexMap<String, T>` where `T: FromTomlItem`
//! to absorb all remaining keys in a table that were not matched by other fields
//! (including their aliases). This is similar to serde's `#[serde(flatten)]`.
//!
//! Key behaviors:
//! 1. The field must be `IndexMap<String, T>` where T: FromTomlItem
//! 2. All keys not matched by explicit fields (including aliases) go into this map
//! 3. Absorbed keys are NOT reported as "unknown" by deny_unknown
//! 4. Only ONE field can have this attribute per struct
//! 5. Works with nested structs when combined with #[tpd_nested]

#![allow(clippy::struct_field_names)]
use indexmap::IndexMap;
use toml_pretty_deser::prelude::*;

// =============================================================================
// Basic absorb_remaining tests
// =============================================================================

/// Basic struct with explicit fields and an absorbing field
#[tpd]
#[derive(Debug)]
struct BasicAbsorb {
    name: String,
    count: u32,
    #[tpd_absorb_remaining]
    extra: IndexMap<String, toml_edit::Item>,
}

#[test]
fn test_absorb_remaining_basic() {
    let toml = r#"
        name = "test"
        count = 42
        unknown_field = "should be absorbed"
        another_unknown = 123
    "#;

    let result = deserialize::<PartialBasicAbsorb, BasicAbsorb>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.name, "test");
        assert_eq!(output.count, 42);
        assert_eq!(output.extra.len(), 2);
        assert!(output.extra.contains_key("unknown_field"));
        assert!(output.extra.contains_key("another_unknown"));
    }
}

#[test]
fn test_absorb_remaining_no_extras() {
    // When all fields are known, extra should be empty
    let toml = r#"
        name = "test"
        count = 42
    "#;

    let result = deserialize::<PartialBasicAbsorb, BasicAbsorb>(toml);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.name, "test");
        assert_eq!(output.count, 42);
        assert!(output.extra.is_empty());
    }
}

#[test]
fn test_absorb_remaining_only_extras() {
    // Explicit fields missing should still be errors, only unknown keys absorbed
    let toml = r#"
        unknown_field = "absorbed"
        another = 456
    "#;

    let result = deserialize::<PartialBasicAbsorb, BasicAbsorb>(toml);
    // Should fail because name and count are missing
    assert!(result.is_err());
    if let Err(DeserError::DeserFailure(errors, partial)) = result {
        // Check that missing required fields are reported
        assert!(errors.iter().any(|e| e.inner.spans[0]
            .msg
            .contains("Missing required key: 'name'")));
        assert!(errors.iter().any(|e| e.inner.spans[0]
            .msg
            .contains("Missing required key: 'count'")));
        // But the unknown fields should NOT cause "Unknown key" errors
        assert!(!errors
            .iter()
            .any(|e| e.inner.spans[0].msg == "Unknown key."));
        // And they should be in the partial's extra field
        assert_eq!(partial.extra.into_option().unwrap().len(), 2);
    }
}

// =============================================================================
// Absorb remaining with aliases
// =============================================================================

/// Struct with aliases - absorbed keys should respect aliases
#[tpd]
#[derive(Debug)]
struct AbsorbWithAliases {
    #[tpd_alias("title", "label")]
    name: String,
    #[tpd_alias("amount", "num")]
    count: u32,
    #[tpd_absorb_remaining]
    extra: IndexMap<String, toml_edit::Item>,
}

#[test]
fn test_absorb_remaining_respects_aliases_primary() {
    // Using primary names - extra should only have truly unknown keys
    let toml = r#"
        name = "test"
        count = 42
        unknown = "value"
    "#;

    let result = deserialize::<PartialAbsorbWithAliases, AbsorbWithAliases>(toml);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.name, "test");
        assert_eq!(output.count, 42);
        assert_eq!(output.extra.len(), 1);
        assert!(output.extra.contains_key("unknown"));
        // Aliases should NOT be in extra since they weren't used
        assert!(!output.extra.contains_key("title"));
        assert!(!output.extra.contains_key("label"));
    }
}

#[test]
fn test_absorb_remaining_respects_aliases_used() {
    // Using aliases - they should NOT be absorbed since they match fields
    let toml = r#"
        title = "using alias"
        amount = 99
        unknown = "value"
    "#;

    let result = deserialize::<PartialAbsorbWithAliases, AbsorbWithAliases>(toml);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.name, "using alias");
        assert_eq!(output.count, 99);
        assert_eq!(output.extra.len(), 1);
        assert!(output.extra.contains_key("unknown"));
        // The aliases used should NOT be in extra
        assert!(!output.extra.contains_key("title"));
        assert!(!output.extra.contains_key("amount"));
    }
}

#[test]
fn test_absorb_remaining_unused_alias_names_not_absorbed() {
    // Keys that happen to match unused alias names should be absorbed
    // (since the actual alias wasn't what matched the field)
    let toml = r#"
        name = "primary name used"
        count = 42
        title = "this is extra because 'name' was used, not 'title'"
        label = "also extra"
    "#;

    let result = deserialize::<PartialAbsorbWithAliases, AbsorbWithAliases>(toml);
    // This should fail because title and label are aliases that conflict with name
    // Actually, they would be absorbed since they weren't the key that matched
    // Wait, this is tricky - let me think...
    // If "name" matched the field, then "title" and "label" are NOT absorbed,
    // because they're defined as aliases. They should cause "Unknown key" error
    // UNLESS absorb_remaining captures them.
    //
    // The semantics should be: absorb_remaining captures keys that don't match
    // ANY expected field's primary name OR aliases.
    // So "title" and "label" match as aliases for "name", hence they're "known"
    // but since "name" is already matched, they might be multi-defined?
    //
    // Actually, I think the correct behavior is:
    // - "title" and "label" ARE aliases for the "name" field
    // - If "name" is already used, then "title"/"label" appearing would be MultiDefined
    // - absorb_remaining only captures keys that match NO field at all
    dbg!(&result);
    assert!(result.is_err());
    if let Err(DeserError::DeserFailure(errors, _)) = result {
        // Should have multi-defined errors for title and label matching name
        let has_multi = errors.iter().any(|e| {
            e.inner.spans.iter().any(|s| {
                s.msg.contains("defined multiple times") || s.msg.contains("Key/alias conflict")
            })
        });
        assert!(has_multi, "Expected multi-defined error");
    }
}

// =============================================================================
// Absorb remaining with typed values
// =============================================================================

/// Absorb with specific type - only valid items go in
#[tpd]
#[derive(Debug)]
struct AbsorbTyped {
    name: String,
    #[tpd_absorb_remaining]
    numbers: IndexMap<String, i64>,
}

#[test]
fn test_absorb_remaining_typed_all_valid() {
    let toml = r#"
        name = "test"
        x = 1
        y = 2
        z = 3
    "#;

    let result = deserialize::<PartialAbsorbTyped, AbsorbTyped>(toml);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.name, "test");
        assert_eq!(output.numbers.len(), 3);
        assert_eq!(output.numbers.get("x"), Some(&1));
        assert_eq!(output.numbers.get("y"), Some(&2));
        assert_eq!(output.numbers.get("z"), Some(&3));
    }
}

#[test]
fn test_absorb_remaining_typed_wrong_type() {
    // When absorbed value has wrong type, it should be an error
    let toml = r#"
        name = "test"
        x = 1
        y = "not a number"
        z = 3
    "#;

    let result = deserialize::<PartialAbsorbTyped, AbsorbTyped>(toml);
    assert!(result.is_err());
    if let Err(DeserError::DeserFailure(errors, _)) = result {
        assert!(errors
            .iter()
            .any(|e| e.inner.spans[0].msg.contains("Wrong type")));
    }
}

// =============================================================================
// Absorb remaining with Vec values (IndexMap<String, Vec<T>>)
// =============================================================================

/// Absorb into IndexMap<String, Vec<String>>
#[tpd]
#[derive(Debug)]
struct AbsorbVecStrings {
    name: String,
    #[tpd_absorb_remaining]
    lists: IndexMap<String, Vec<String>>,
}

#[test]
fn test_absorb_remaining_vec_strings_arrays() {
    let toml = r#"
        name = "test"
        tags = ["alpha", "beta", "gamma"]
        categories = ["one", "two"]
    "#;

    let result = deserialize::<PartialAbsorbVecStrings, AbsorbVecStrings>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.name, "test");
        assert_eq!(output.lists.len(), 2);
        assert_eq!(
            output.lists.get("tags"),
            Some(&vec![
                "alpha".to_string(),
                "beta".to_string(),
                "gamma".to_string()
            ])
        );
        assert_eq!(
            output.lists.get("categories"),
            Some(&vec!["one".to_string(), "two".to_string()])
        );
    }
}

#[test]
fn test_absorb_remaining_vec_strings_single_ok_mode() {
    // With VecMode::SingleOk, single values should be promoted to Vec
    let toml = r#"
        name = "test"
        single_tag = "just-one"
        multi_tags = ["a", "b"]
    "#;

    let result = deserialize_with_mode::<PartialAbsorbVecStrings, AbsorbVecStrings>(
        toml,
        FieldMatchMode::Exact,
        VecMode::SingleOk,
    );
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.name, "test");
        assert_eq!(output.lists.len(), 2);
        assert_eq!(
            output.lists.get("single_tag"),
            Some(&vec!["just-one".to_string()])
        );
        assert_eq!(
            output.lists.get("multi_tags"),
            Some(&vec!["a".to_string(), "b".to_string()])
        );
    }
}

#[test]
fn test_absorb_remaining_vec_strings_empty_array() {
    let toml = r#"
        name = "test"
        empty = []
        non_empty = ["value"]
    "#;

    let result = deserialize::<PartialAbsorbVecStrings, AbsorbVecStrings>(toml);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.name, "test");
        assert_eq!(output.lists.len(), 2);
        assert_eq!(output.lists.get("empty"), Some(&vec![]));
        assert_eq!(
            output.lists.get("non_empty"),
            Some(&vec!["value".to_string()])
        );
    }
}

#[test]
fn test_absorb_remaining_vec_strings_wrong_element_type() {
    // Array with wrong element types should error
    let toml = r#"
        name = "test"
        numbers = [1, 2, 3]
    "#;

    let result = deserialize::<PartialAbsorbVecStrings, AbsorbVecStrings>(toml);
    assert!(result.is_err());
    if let Err(DeserError::DeserFailure(errors, _)) = result {
        assert!(errors
            .iter()
            .any(|e| e.inner.spans[0].msg.contains("Wrong type")));
    }
}

#[test]
fn test_absorb_remaining_vec_strings_mixed_valid_invalid() {
    // Some arrays valid, some invalid
    let toml = r#"
        name = "test"
        good = ["a", "b"]
        bad = [1, 2, 3]
        also_good = ["c"]
    "#;

    let result = deserialize::<PartialAbsorbVecStrings, AbsorbVecStrings>(toml);
    assert!(result.is_err());
    if let Err(DeserError::DeserFailure(errors, partial)) = result {
        // Should have error for 'bad'
        assert!(errors
            .iter()
            .any(|e| e.inner.spans[0].msg.contains("Wrong type")));
        // Partial should still have the good ones (access via .value, not .into_option())
        let lists = partial.lists.value;
        assert!(lists.is_some());
        let lists = lists.unwrap();
        assert!(lists.contains_key("good"));
        assert!(lists.contains_key("also_good"));
    }
}

/// Absorb into IndexMap<String, Vec<i32>>
#[tpd]
#[derive(Debug)]
struct AbsorbVecNumbers {
    name: String,
    #[tpd_absorb_remaining]
    number_lists: IndexMap<String, Vec<i32>>,
}

#[test]
fn test_absorb_remaining_vec_numbers() {
    let toml = r#"
        name = "test"
        scores = [100, 95, 87]
        counts = [1, 2, 3, 4, 5]
    "#;

    let result = deserialize::<PartialAbsorbVecNumbers, AbsorbVecNumbers>(toml);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.name, "test");
        assert_eq!(output.number_lists.len(), 2);
        assert_eq!(output.number_lists.get("scores"), Some(&vec![100, 95, 87]));
        assert_eq!(
            output.number_lists.get("counts"),
            Some(&vec![1, 2, 3, 4, 5])
        );
    }
}

#[test]
fn test_absorb_remaining_vec_preserves_order() {
    let toml = r#"
        name = "test"
        first = ["a"]
        second = ["b"]
        third = ["c"]
        fourth = ["d"]
    "#;

    let result = deserialize::<PartialAbsorbVecStrings, AbsorbVecStrings>(toml);
    assert!(result.is_ok());
    if let Ok(output) = result {
        let keys: Vec<&String> = output.lists.keys().collect();
        assert_eq!(keys, vec!["first", "second", "third", "fourth"]);
    }
}

// =============================================================================
// Absorb remaining with nested structs
// =============================================================================

#[tpd]
#[derive(Debug, Clone)]
struct InnerConfig {
    value: i32,
    enabled: bool,
}

#[tpd]
#[derive(Debug)]
struct AbsorbNested {
    name: String,
    #[tpd_nested]
    #[tpd_absorb_remaining]
    configs: IndexMap<String, InnerConfig>,
}

#[test]
fn test_absorb_remaining_nested() {
    let toml = r#"
        name = "test"
        
        [server]
        value = 8080
        enabled = true
        
        [database]
        value = 5432
        enabled = false
    "#;

    let result = deserialize::<PartialAbsorbNested, AbsorbNested>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.name, "test");
        assert_eq!(output.configs.len(), 2);
        assert!(output.configs.contains_key("server"));
        assert!(output.configs.contains_key("database"));
        assert_eq!(output.configs.get("server").unwrap().value, 8080);
        assert!(output.configs.get("server").unwrap().enabled);
        assert_eq!(output.configs.get("database").unwrap().value, 5432);
        assert!(!output.configs.get("database").unwrap().enabled);
    }
}

#[test]
fn test_absorb_remaining_nested_error_propagation() {
    // Errors in nested structs should still be reported
    let toml = r#"
        name = "test"
        
        [server]
        value = 8080
        # missing enabled
        
        [database]
        value = "wrong type"
        enabled = false
    "#;

    let result = deserialize::<PartialAbsorbNested, AbsorbNested>(toml);
    assert!(result.is_err());
    if let Err(DeserError::DeserFailure(errors, _)) = result {
        // Should have error about missing 'enabled' in server
        assert!(errors.iter().any(|e| e
            .inner
            .spans
            .iter()
            .any(|s| s.msg.contains("Missing required key: 'enabled'"))));
        // Should have error about wrong type for value in database
        assert!(errors
            .iter()
            .any(|e| e.inner.spans.iter().any(|s| s.msg.contains("Wrong type"))));
    }
}

// =============================================================================
// Absorb remaining with Option
// =============================================================================

#[tpd]
#[derive(Debug)]
struct AbsorbOptional {
    name: String,
    #[tpd_absorb_remaining]
    extra: Option<IndexMap<String, String>>,
}

#[test]
fn test_absorb_remaining_optional_with_extras() {
    let toml = r#"
        name = "test"
        foo = "bar"
        baz = "qux"
    "#;

    let result = deserialize::<PartialAbsorbOptional, AbsorbOptional>(toml);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.name, "test");
        assert!(output.extra.is_some());
        let extra = output.extra.unwrap();
        assert_eq!(extra.len(), 2);
        assert_eq!(extra.get("foo"), Some(&"bar".to_string()));
    }
}

#[test]
fn test_absorb_remaining_optional_without_extras() {
    let toml = r#"
        name = "test"
    "#;

    let result = deserialize::<PartialAbsorbOptional, AbsorbOptional>(toml);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.name, "test");
        // Could be Some(empty map) or None - implementation decides
        // For consistency, probably Some(empty map) is better
        assert!(output.extra.is_none() || output.extra.as_ref().unwrap().is_empty());
    }
}

// =============================================================================
// Absorb remaining preserves order
// =============================================================================

#[tpd]
#[derive(Debug)]
struct AbsorbOrdered {
    required: String,
    #[tpd_absorb_remaining]
    extra: IndexMap<String, String>,
}

#[test]
fn test_absorb_remaining_preserves_order() {
    let toml = r#"
        alpha = "first"
        required = "middle"
        beta = "second"
        gamma = "third"
        delta = "fourth"
    "#;

    let result = deserialize::<PartialAbsorbOrdered, AbsorbOrdered>(toml);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.required, "middle");
        let keys: Vec<&String> = output.extra.keys().collect();
        // Should preserve TOML order
        assert_eq!(keys, vec!["alpha", "beta", "gamma", "delta"]);
    }
}

// =============================================================================
// Absorb remaining with field match modes
// =============================================================================

#[tpd]
#[derive(Debug)]
struct AbsorbCaseMode {
    my_field: String,
    #[tpd_absorb_remaining]
    extra: IndexMap<String, String>,
}

#[test]
fn test_absorb_remaining_exact_mode() {
    let toml = r#"
        my_field = "exact"
        MY_FIELD_UPPER = "should be absorbed in exact mode"
        other = "also absorbed"
    "#;

    let result = deserialize_with_mode::<PartialAbsorbCaseMode, AbsorbCaseMode>(
        toml,
        FieldMatchMode::Exact,
        VecMode::Strict,
    );
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.my_field, "exact");
        assert_eq!(output.extra.len(), 2);
        assert!(output.extra.contains_key("MY_FIELD_UPPER"));
        assert!(output.extra.contains_key("other"));
    }
}

#[test]
fn test_absorb_remaining_upper_lower_mode() {
    // In UpperLower mode, MY_FIELD should match my_field and NOT be absorbed
    let toml = r#"
        MY_FIELD = "case insensitive match"
        other = "absorbed"
    "#;

    let result = deserialize_with_mode::<PartialAbsorbCaseMode, AbsorbCaseMode>(
        toml,
        FieldMatchMode::UpperLower,
        VecMode::Strict,
    );
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.my_field, "case insensitive match");
        assert_eq!(output.extra.len(), 1);
        assert!(output.extra.contains_key("other"));
        // MY_FIELD should NOT be in extra because it matched my_field
        assert!(!output.extra.contains_key("MY_FIELD"));
    }
}

// =============================================================================
// Absorb remaining with defaults
// =============================================================================

#[tpd]
#[derive(Debug)]
struct AbsorbWithDefault {
    name: String,
    #[tpd_default]
    count: u32,
    #[tpd_absorb_remaining]
    extra: IndexMap<String, toml_edit::Item>,
}

#[test]
fn test_absorb_remaining_with_default_field() {
    let toml = r#"
        name = "test"
        extra_field = "absorbed"
    "#;

    let result = deserialize::<PartialAbsorbWithDefault, AbsorbWithDefault>(toml);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.name, "test");
        assert_eq!(output.count, 0); // default
        assert_eq!(output.extra.len(), 1);
        assert!(output.extra.contains_key("extra_field"));
    }
}

// =============================================================================
// Inline tables
// =============================================================================

#[test]
fn test_absorb_remaining_inline_table() {
    let toml = r#"
        name = "test"
        count = 5
        foo = "bar"
        nested = { a = 1, b = 2 }
    "#;

    let result = deserialize::<PartialBasicAbsorb, BasicAbsorb>(toml);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.name, "test");
        assert_eq!(output.count, 5);
        assert_eq!(output.extra.len(), 2);
        assert!(output.extra.contains_key("foo"));
        assert!(output.extra.contains_key("nested"));
    }
}

// =============================================================================
// Error cases - compile time checks are in compilation_failure_tests/
// See: absorb_remaining_multiple.rs, absorb_remaining_wrong_type_*.rs
// =============================================================================

// =============================================================================
// Absorb remaining with skip fields
// =============================================================================

#[tpd]
#[derive(Debug)]
struct AbsorbWithSkip {
    name: String,
    #[tpd_skip]
    internal: u32,
    #[tpd_absorb_remaining]
    extra: IndexMap<String, toml_edit::Item>,
}

#[test]
fn test_absorb_remaining_skipped_field_name_absorbed() {
    // A key matching a skipped field's name should be absorbed
    // since the skipped field doesn't "use" that key from TOML
    let toml = r#"
        name = "test"
        internal = 999
        other = "value"
    "#;

    let result = deserialize::<PartialAbsorbWithSkip, AbsorbWithSkip>(toml);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.name, "test");
        assert_eq!(output.internal, 0); // default, not from TOML
                                        // "internal" should be absorbed since the field is skipped
        assert!(output.extra.contains_key("internal"));
        assert!(output.extra.contains_key("other"));
        assert_eq!(output.extra.len(), 2);
    }
}

// =============================================================================
// Complex scenario: combining many features
// =============================================================================

#[tpd]
#[derive(Debug)]
struct ComplexAbsorb {
    #[tpd_alias("identifier", "id")]
    name: String,
    #[tpd_default]
    count: u32,
    #[tpd_skip]
    internal_state: bool,
    opt_field: Option<String>,
    #[tpd_absorb_remaining]
    extra: IndexMap<String, toml_edit::Item>,
}

#[test]
fn test_absorb_remaining_complex_scenario() {
    let toml = r#"
        identifier = "using alias"
        internal_state = true
        custom_setting = "absorbed"
        another_custom = 123
    "#;

    let result = deserialize::<PartialComplexAbsorb, ComplexAbsorb>(toml);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.name, "using alias");
        assert_eq!(output.count, 0); // default
        assert!(!output.internal_state); // skipped, uses Default
        assert!(output.opt_field.is_none());
        // internal_state is skipped, so its TOML key is absorbed
        assert!(output.extra.contains_key("internal_state"));
        assert!(output.extra.contains_key("custom_setting"));
        assert!(output.extra.contains_key("another_custom"));
        assert_eq!(output.extra.len(), 3);
    }
}

// =============================================================================
// Snapshot tests for error messages
// =============================================================================

#[test]
fn test_absorb_remaining_error_snapshot() {
    let toml = r#"
        name = "test"
        bad_number = "not a number"
    "#;

    let result = deserialize::<PartialAbsorbTyped, AbsorbTyped>(toml);
    if let Err(e) = result {
        insta::assert_snapshot!(e.pretty("test.toml"));
    } else {
        panic!("Expected error");
    }
}
