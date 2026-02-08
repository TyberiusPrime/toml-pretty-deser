#![allow(dead_code)]
use indexmap::IndexMap;
use toml_pretty_deser::prelude::*;

// Test basic tpd_with usage - convert string to custom type
#[derive(Debug, PartialEq, Eq)]
struct UppercaseString(String);

fn parse_uppercase(s: &str) -> Result<UppercaseString, WithError> {
    Ok(UppercaseString(s.to_uppercase()))
}

#[tpd]
#[derive(Debug)]
struct WithUppercase {
    #[tpd_with(parse_uppercase)]
    name: UppercaseString,
    count: i32,
}

#[test]
fn test_tpd_with_basic_happy_path() {
    let toml = r#"
        name = "hello world"
        count = 42
    "#;

    let result: Result<_, _> = deserialize::<PartialWithUppercase, WithUppercase>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.name, UppercaseString("HELLO WORLD".to_string()));
        assert_eq!(output.count, 42);
    }
}

// Test tpd_with with validation that can fail
#[derive(Debug, PartialEq, Eq)]
struct PositiveInt(i32);

fn parse_positive(s: &str) -> Result<PositiveInt, WithError> {
    match s.parse::<i32>() {
        Ok(n) if n > 0 => Ok(PositiveInt(n)),
        Ok(_) => Err((
            "Value must be positive".to_string(),
            Some("Use a number greater than 0".to_string()),
        )),
        Err(_) => Err((
            "Invalid integer format".to_string(),
            Some("Expected a valid integer string".to_string()),
        )),
    }
}

#[tpd]
#[derive(Debug)]
struct WithPositiveInt {
    #[tpd_with(parse_positive)]
    value: PositiveInt,
}

#[test]
fn test_tpd_with_validation_success() {
    let toml = r#"value = "42""#;

    let result: Result<_, _> = deserialize::<PartialWithPositiveInt, WithPositiveInt>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.value, PositiveInt(42));
    }
}

#[test]
fn test_tpd_with_validation_failure_negative() {
    let toml = r#"value = "-5""#;

    let result: Result<_, _> = deserialize::<PartialWithPositiveInt, WithPositiveInt>(toml);
    dbg!(&result);
    assert!(result.is_err());
    if let Err(e) = result {
        let err_str = format!("{:?}", e);
        assert!(err_str.contains("must be positive"));
    }
}

#[test]
fn test_tpd_with_validation_failure_not_integer() {
    let toml = r#"value = "not_a_number""#;

    let result: Result<_, _> = deserialize::<PartialWithPositiveInt, WithPositiveInt>(toml);
    dbg!(&result);
    assert!(result.is_err());
    if let Err(e) = result {
        let err_str = format!("{:?}", e);
        assert!(err_str.contains("Invalid integer"));
    }
}

// Test tpd_with with wrong input type (not a string)
#[test]
fn test_tpd_with_wrong_input_type() {
    let toml = "value = 123"; // integer, not string

    let result: Result<_, _> = deserialize::<PartialWithPositiveInt, WithPositiveInt>(toml);
    dbg!(&result);
    assert!(result.is_err());
    if let Err(e) = result {
        let err_str = format!("{:?}", e);
        assert!(err_str.contains("Wrong type"));
    }
}

// Test tpd_with with path syntax (module::function)
mod converters {
    use super::*;

    pub fn to_lowercase(s: &str) -> Result<String, WithError> {
        Ok(s.to_lowercase())
    }
}

#[tpd]
#[derive(Debug)]
struct WithPathFunction {
    #[tpd_with(converters::to_lowercase)]
    name: String,
}

#[test]
fn test_tpd_with_path_function() {
    let toml = r#"name = "HELLO""#;

    let result: Result<_, _> = deserialize::<PartialWithPathFunction, WithPathFunction>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.name, "hello");
    }
}

// Test tpd_with combined with tpd_default
#[derive(Debug, PartialEq, Eq, Default)]
struct DefaultableCustom(String);

fn parse_defaultable(s: &str) -> Result<DefaultableCustom, WithError> {
    Ok(DefaultableCustom(s.to_string()))
}

#[tpd]
#[derive(Debug)]
struct WithDefaultAndWith {
    #[tpd_default]
    #[tpd_with(parse_defaultable)]
    custom: DefaultableCustom,
    required: i32,
}

#[test]
fn test_tpd_with_and_default_present() {
    let toml = r#"
        custom = "provided"
        required = 10
    "#;

    let result: Result<_, _> = deserialize::<PartialWithDefaultAndWith, WithDefaultAndWith>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.custom, DefaultableCustom("provided".to_string()));
    }
}

#[test]
fn test_tpd_with_and_default_missing() {
    let toml = "required = 10";

    let result: Result<_, _> = deserialize::<PartialWithDefaultAndWith, WithDefaultAndWith>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.custom, DefaultableCustom::default());
    }
}

// Test tpd_with combined with tpd_alias
#[tpd]
#[derive(Debug)]
struct WithAliasAndWith {
    #[tpd_alias("title")]
    #[tpd_with(parse_uppercase)]
    name: UppercaseString,
}

#[test]
fn test_tpd_with_and_alias_primary() {
    let toml = r#"name = "hello""#;

    let result: Result<_, _> = deserialize::<PartialWithAliasAndWith, WithAliasAndWith>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.name, UppercaseString("HELLO".to_string()));
    }
}

#[test]
fn test_tpd_with_and_alias_secondary() {
    let toml = r#"title = "world""#;

    let result: Result<_, _> = deserialize::<PartialWithAliasAndWith, WithAliasAndWith>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.name, UppercaseString("WORLD".to_string()));
    }
}

// Test missing required field with tpd_with
#[test]
fn test_tpd_with_missing_required() {
    let toml = "count = 42"; // name is missing

    let result: Result<_, _> = deserialize::<PartialWithUppercase, WithUppercase>(toml);
    dbg!(&result);
    assert!(result.is_err());
    if let Err(e) = result {
        let err_str = format!("{:?}", e);
        assert!(err_str.contains("Missing required key"));
    }
}

// Test that missing tpd_with key error is not duplicated
// This test verifies the bug where the error was added twice
#[test]
fn test_tpd_with_missing_key_error_not_duplicated() {
    let toml = "count = 42"; // name is missing

    let result: Result<_, _> = deserialize::<PartialWithUppercase, WithUppercase>(toml);
    assert!(result.is_err());
    if let Err(e) = result {
        let err_str = format!("{:?}", e);
        // Count occurrences of "Missing required key" in the error string
        let count = err_str.matches("Missing required key").count();
        assert_eq!(
            count, 1,
            "Expected 'Missing required key' to appear exactly once, but it appeared {} times. Full error: {}",
            count, err_str
        );
    }
}

// Test with closure-like inline functions (using a helper)
fn make_prefixed_parser(prefix: &'static str) -> impl Fn(&str) -> Result<String, WithError> {
    move |s| Ok(format!("{}{}", prefix, s))
}

// Note: We can't use closures directly in tpd_with, but we can use function pointers
fn add_prefix(s: &str) -> Result<String, WithError> {
    Ok(format!("PREFIX_{}", s))
}

#[tpd]
#[derive(Debug)]
struct WithPrefixer {
    #[tpd_with(add_prefix)]
    value: String,
}

#[test]
fn test_tpd_with_prefixer() {
    let toml = r#"value = "test""#;

    let result: Result<_, _> = deserialize::<PartialWithPrefixer, WithPrefixer>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.value, "PREFIX_test");
    }
}

// ====================================
// Tests for Option<T> with tpd_with
// ====================================

#[tpd]
#[derive(Debug)]
struct WithOptionalCustom {
    #[tpd_with(parse_uppercase)]
    name: Option<UppercaseString>,
    count: i32,
}

#[test]
fn test_tpd_with_option_present() {
    let toml = r#"
        name = "hello"
        count = 42
    "#;

    let result: Result<_, _> = deserialize::<PartialWithOptionalCustom, WithOptionalCustom>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.name, Some(UppercaseString("HELLO".to_string())));
        assert_eq!(output.count, 42);
    }
}

#[test]
fn test_tpd_with_option_missing() {
    let toml = r#"count = 42"#;

    let result: Result<_, _> = deserialize::<PartialWithOptionalCustom, WithOptionalCustom>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.name, None);
        assert_eq!(output.count, 42);
    }
}

#[test]
fn test_tpd_with_option_validation_failure() {
    // Use a converter that can fail
    let toml = r#"
        value = "-5"
    "#;

    #[tpd]
    #[derive(Debug)]
    struct OptionalPositive {
        #[tpd_with(parse_positive)]
        value: Option<PositiveInt>,
    }

    let result: Result<_, _> = deserialize::<PartialOptionalPositive, OptionalPositive>(toml);
    dbg!(&result);
    assert!(result.is_err());
    if let Err(e) = result {
        let err_str = format!("{:?}", e);
        assert!(err_str.contains("must be positive"));
    }
}

#[test]
fn test_tpd_with_option_wrong_type() {
    let toml = r#"
        name = 123
        count = 42
    "#;

    let result: Result<_, _> = deserialize::<PartialWithOptionalCustom, WithOptionalCustom>(toml);
    dbg!(&result);
    assert!(result.is_err());
    if let Err(e) = result {
        let err_str = format!("{:?}", e);
        assert!(err_str.contains("Wrong type"));
    }
}

// ====================================
// Tests for IndexMap<String, T> with tpd_with
// ====================================

#[tpd]
#[derive(Debug)]
struct WithMapCustom {
    #[tpd_with(parse_uppercase)]
    names: IndexMap<String, UppercaseString>,
}

#[test]
fn test_tpd_with_indexmap_happy_path() {
    let toml = r#"
        [names]
        first = "alice"
        second = "bob"
    "#;

    let result: Result<_, _> = deserialize::<PartialWithMapCustom, WithMapCustom>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.names.len(), 2);
        assert_eq!(
            output.names.get("first"),
            Some(&UppercaseString("ALICE".to_string()))
        );
        assert_eq!(
            output.names.get("second"),
            Some(&UppercaseString("BOB".to_string()))
        );
    }
}

#[test]
fn test_tpd_with_indexmap_inline_table() {
    let toml = r#"names = { first = "alice", second = "bob" }"#;

    let result: Result<_, _> = deserialize::<PartialWithMapCustom, WithMapCustom>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.names.len(), 2);
        assert_eq!(
            output.names.get("first"),
            Some(&UppercaseString("ALICE".to_string()))
        );
    }
}

#[test]
fn test_tpd_with_indexmap_validation_failure() {
    #[tpd]
    #[derive(Debug)]
    struct MapPositive {
        #[tpd_with(parse_positive)]
        values: IndexMap<String, PositiveInt>,
    }

    let toml = r#"
        [values]
        good = "5"
        bad = "-10"
    "#;

    let result: Result<_, _> = deserialize::<PartialMapPositive, MapPositive>(toml);
    dbg!(&result);
    assert!(result.is_err());
    if let Err(e) = result {
        let err_str = format!("{:?}", e);
        assert!(err_str.contains("must be positive"));
    }
}

#[test]
fn test_tpd_with_indexmap_wrong_value_type() {
    let toml = r#"
        [names]
        first = "alice"
        second = 123
    "#;

    let result: Result<_, _> = deserialize::<PartialWithMapCustom, WithMapCustom>(toml);
    dbg!(&result);
    assert!(result.is_err());
    if let Err(e) = result {
        let err_str = format!("{:?}", e);
        assert!(err_str.contains("Wrong type"));
    }
}

#[test]
fn test_tpd_with_indexmap_missing() {
    // Missing required IndexMap field should error
    let toml = "";

    let result: Result<_, _> = deserialize::<PartialWithMapCustom, WithMapCustom>(toml);
    dbg!(&result);
    assert!(result.is_err());
    if let Err(e) = result {
        let err_str = format!("{:?}", e);
        assert!(err_str.contains("Missing required key"));
    }
}

// ====================================
// Tests for Option<IndexMap<String, T>> with tpd_with
// ====================================

#[tpd]
#[derive(Debug)]
struct WithOptionalMapCustom {
    #[tpd_with(parse_uppercase)]
    names: Option<IndexMap<String, UppercaseString>>,
    count: i32,
}

#[test]
fn test_tpd_with_optional_indexmap_present() {
    let toml = r#"
        count = 10
        [names]
        first = "alice"
    "#;

    let result: Result<_, _> =
        deserialize::<PartialWithOptionalMapCustom, WithOptionalMapCustom>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert!(output.names.is_some());
        let names = output.names.unwrap();
        assert_eq!(names.len(), 1);
        assert_eq!(
            names.get("first"),
            Some(&UppercaseString("ALICE".to_string()))
        );
    }
}

#[test]
fn test_tpd_with_optional_indexmap_missing() {
    let toml = r#"count = 10"#;

    let result: Result<_, _> =
        deserialize::<PartialWithOptionalMapCustom, WithOptionalMapCustom>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert!(output.names.is_none());
        assert_eq!(output.count, 10);
    }
}

// ====================================
// Tests for Vec<T> with tpd_with
// ====================================

#[tpd]
#[derive(Debug)]
struct WithVecCustom {
    #[tpd_with(parse_uppercase)]
    names: Vec<UppercaseString>,
}

#[test]
fn test_tpd_with_vec_happy_path() {
    let toml = r#"names = ["alice", "bob", "carol"]"#;

    let result: Result<_, _> = deserialize::<PartialWithVecCustom, WithVecCustom>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.names.len(), 3);
        assert_eq!(output.names[0], UppercaseString("ALICE".to_string()));
        assert_eq!(output.names[1], UppercaseString("BOB".to_string()));
        assert_eq!(output.names[2], UppercaseString("CAROL".to_string()));
    }
}

#[test]
fn test_tpd_with_vec_empty() {
    let toml = r#"names = []"#;

    let result: Result<_, _> = deserialize::<PartialWithVecCustom, WithVecCustom>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.names.len(), 0);
    }
}

#[test]
fn test_tpd_with_vec_validation_failure() {
    #[tpd]
    #[derive(Debug)]
    struct VecPositive {
        #[tpd_with(parse_positive)]
        values: Vec<PositiveInt>,
    }

    let toml = r#"values = ["5", "-10", "3"]"#;

    let result: Result<_, _> = deserialize::<PartialVecPositive, VecPositive>(toml);
    dbg!(&result);
    assert!(result.is_err());
    if let Err(e) = result {
        let err_str = format!("{:?}", e);
        assert!(err_str.contains("must be positive"));
    }
}

#[test]
fn test_tpd_with_vec_wrong_element_type() {
    let toml = r#"names = ["alice", 123, "carol"]"#;

    let result: Result<_, _> = deserialize::<PartialWithVecCustom, WithVecCustom>(toml);
    dbg!(&result);
    assert!(result.is_err());
    if let Err(e) = result {
        let err_str = format!("{:?}", e);
        assert!(err_str.contains("Wrong type"));
    }
}

#[test]
fn test_tpd_with_vec_missing() {
    // Missing required Vec field should error
    let toml = "";

    let result: Result<_, _> = deserialize::<PartialWithVecCustom, WithVecCustom>(toml);
    dbg!(&result);
    assert!(result.is_err());
    if let Err(e) = result {
        let err_str = format!("{:?}", e);
        assert!(err_str.contains("Missing required key"));
    }
}

// ====================================
// Tests for Option<Vec<T>> with tpd_with
// ====================================

#[tpd]
#[derive(Debug)]
struct WithOptionalVecCustom {
    #[tpd_with(parse_uppercase)]
    names: Option<Vec<UppercaseString>>,
    count: i32,
}

#[test]
fn test_tpd_with_option_vec_present() {
    let toml = r#"
        names = ["alice", "bob"]
        count = 42
    "#;

    let result: Result<_, _> =
        deserialize::<PartialWithOptionalVecCustom, WithOptionalVecCustom>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert!(output.names.is_some());
        let names = output.names.unwrap();
        assert_eq!(names.len(), 2);
        assert_eq!(names[0], UppercaseString("ALICE".to_string()));
        assert_eq!(names[1], UppercaseString("BOB".to_string()));
        assert_eq!(output.count, 42);
    }
}

#[test]
fn test_tpd_with_option_vec_missing() {
    let toml = r#"count = 42"#;

    let result: Result<_, _> =
        deserialize::<PartialWithOptionalVecCustom, WithOptionalVecCustom>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert!(output.names.is_none());
        assert_eq!(output.count, 42);
    }
}

#[test]
fn test_tpd_with_option_vec_empty() {
    let toml = r#"
        names = []
        count = 42
    "#;

    let result: Result<_, _> =
        deserialize::<PartialWithOptionalVecCustom, WithOptionalVecCustom>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert!(output.names.is_some());
        assert_eq!(output.names.unwrap().len(), 0);
    }
}

#[test]
fn test_tpd_with_option_vec_validation_failure() {
    #[tpd]
    #[derive(Debug)]
    struct OptionalVecPositive {
        #[tpd_with(parse_positive)]
        values: Option<Vec<PositiveInt>>,
    }

    let toml = r#"values = ["5", "-10"]"#;

    let result: Result<_, _> = deserialize::<PartialOptionalVecPositive, OptionalVecPositive>(toml);
    dbg!(&result);
    assert!(result.is_err());
    if let Err(e) = result {
        let err_str = format!("{:?}", e);
        assert!(err_str.contains("must be positive"));
    }
}

// ====================================
// Tests for Option<Box<T>> with tpd_with
// ====================================

#[tpd]
#[derive(Debug)]
struct WithOptionalBoxedCustom {
    #[tpd_with(parse_uppercase)]
    name: Option<Box<UppercaseString>>,
    count: i32,
}

#[test]
fn test_tpd_with_option_box_present() {
    let toml = r#"
        name = "hello"
        count = 42
    "#;

    let result: Result<_, _> =
        deserialize::<PartialWithOptionalBoxedCustom, WithOptionalBoxedCustom>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert!(output.name.is_some());
        assert_eq!(*output.name.unwrap(), UppercaseString("HELLO".to_string()));
        assert_eq!(output.count, 42);
    }
}

#[test]
fn test_tpd_with_option_box_missing() {
    let toml = r#"count = 42"#;

    let result: Result<_, _> =
        deserialize::<PartialWithOptionalBoxedCustom, WithOptionalBoxedCustom>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert!(output.name.is_none());
        assert_eq!(output.count, 42);
    }
}

#[test]
fn test_tpd_with_option_box_validation_failure() {
    #[tpd]
    #[derive(Debug)]
    struct OptionalBoxedPositive {
        #[tpd_with(parse_positive)]
        value: Option<Box<PositiveInt>>,
    }

    let toml = r#"value = "-5""#;

    let result: Result<_, _> =
        deserialize::<PartialOptionalBoxedPositive, OptionalBoxedPositive>(toml);
    dbg!(&result);
    assert!(result.is_err());
    if let Err(e) = result {
        let err_str = format!("{:?}", e);
        assert!(err_str.contains("must be positive"));
    }
}

// ====================================
// Tests for Box<T> with tpd_with
// ====================================

#[tpd]
#[derive(Debug)]
struct WithBoxedCustom {
    #[tpd_with(parse_uppercase)]
    name: Box<UppercaseString>,
}

#[test]
fn test_tpd_with_box_happy_path() {
    let toml = r#"name = "hello""#;

    let result: Result<_, _> = deserialize::<PartialWithBoxedCustom, WithBoxedCustom>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(*output.name, UppercaseString("HELLO".to_string()));
    }
}

#[test]
fn test_tpd_with_box_validation_failure() {
    #[tpd]
    #[derive(Debug)]
    struct BoxedPositive {
        #[tpd_with(parse_positive)]
        value: Box<PositiveInt>,
    }

    let toml = r#"value = "-5""#;

    let result: Result<_, _> = deserialize::<PartialBoxedPositive, BoxedPositive>(toml);
    dbg!(&result);
    assert!(result.is_err());
    if let Err(e) = result {
        let err_str = format!("{:?}", e);
        assert!(err_str.contains("must be positive"));
    }
}

// =============================================================================
// Tests for tpd_with combined with tpd_default_in_verify
// =============================================================================

// This type does NOT implement Default, so we must set it in verify()
#[derive(Debug, PartialEq, Eq)]
struct ComputedCustom(String);

fn parse_computed(s: &str) -> Result<ComputedCustom, WithError> {
    Ok(ComputedCustom(s.to_uppercase()))
}

#[tpd(partial = false)]
#[derive(Debug)]
struct WithDefaultInVerifyAndWith {
    #[tpd_default_in_verify]
    #[tpd_with(parse_computed)]
    custom: ComputedCustom,
    required: i32,
}

impl VerifyFromToml for PartialWithDefaultInVerifyAndWith {
    fn verify(mut self, _helper: &mut TomlHelper<'_>) -> Self {
        // If custom is missing, provide a computed default
        if self.custom.value.is_none() {
            self.custom = TomlValue::new_ok(
                ComputedCustom("DEFAULT_COMPUTED".to_string()),
                self.custom.span(),
            );
        }
        self
    }
}

#[test]
fn test_tpd_with_and_default_in_verify_present() {
    let toml = r#"
        custom = "provided"
        required = 10
    "#;

    let result: Result<_, _> =
        deserialize::<PartialWithDefaultInVerifyAndWith, WithDefaultInVerifyAndWith>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.custom, ComputedCustom("PROVIDED".to_string()));
        assert_eq!(output.required, 10);
    }
}

#[test]
fn test_tpd_with_and_default_in_verify_missing() {
    // This is the key test case: when the field is missing, we should NOT get
    // a "Missing required key" error because tpd_default_in_verify indicates
    // we'll provide a value in verify()
    let toml = "required = 10";

    let result: Result<_, _> =
        deserialize::<PartialWithDefaultInVerifyAndWith, WithDefaultInVerifyAndWith>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(
            output.custom,
            ComputedCustom("DEFAULT_COMPUTED".to_string())
        );
        assert_eq!(output.required, 10);
    }
}

// =============================================================================
// Tests for VecMode::SingleOk with tpd_with
// =============================================================================

#[test]
fn test_tpd_with_vec_single_ok_mode_single_value() {
    // When using VecMode::SingleOk, a single string value should be treated as [value]
    #[tpd]
    #[derive(Debug)]
    struct WithVecSingleOk {
        #[tpd_with(parse_uppercase)]
        items: Vec<UppercaseString>,
    }

    let toml = "items = 'hello'"; // Single value, not an array

    let result = deserialize_with_mode::<PartialWithVecSingleOk, WithVecSingleOk>(
        toml,
        FieldMatchMode::Exact,
        VecMode::SingleOk,
    );
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.items.len(), 1);
        assert_eq!(output.items[0].0, "HELLO");
    }
}

#[test]
fn test_tpd_with_vec_single_ok_mode_array_value() {
    // VecMode::SingleOk should still work with arrays
    #[tpd]
    #[derive(Debug)]
    struct WithVecSingleOkArray {
        #[tpd_with(parse_uppercase)]
        items: Vec<UppercaseString>,
    }

    let toml = "items = ['hello', 'world']";

    let result = deserialize_with_mode::<PartialWithVecSingleOkArray, WithVecSingleOkArray>(
        toml,
        FieldMatchMode::Exact,
        VecMode::SingleOk,
    );
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.items.len(), 2);
        assert_eq!(output.items[0].0, "HELLO");
        assert_eq!(output.items[1].0, "WORLD");
    }
}

// =============================================================================
// Tests for FieldMatchMode with tpd_with (case insensitivity)
// =============================================================================

#[test]
fn test_tpd_with_field_match_mode_anycase() {
    // When using FieldMatchMode::AnyCase, field names should be case insensitive
    #[tpd]
    #[derive(Debug)]
    struct WithFieldMatchMode {
        #[tpd_with(parse_uppercase)]
        my_field: UppercaseString,
    }

    let toml = "MyField = 'hello'"; // Different case than my_field

    let result = deserialize_with_mode::<PartialWithFieldMatchMode, WithFieldMatchMode>(
        toml,
        FieldMatchMode::AnyCase,
        VecMode::Strict,
    );
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.my_field.0, "HELLO");
    }
}
