#![allow(dead_code)]
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
