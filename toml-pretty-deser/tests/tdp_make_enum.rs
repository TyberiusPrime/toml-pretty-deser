use toml_pretty_deser::prelude::*;

/// Test enum using the new #[tpd_make_enum] macro.
/// This should work WITHOUT #[as_enum] on fields!
#[tpd_make_enum]
#[derive(Debug, Clone, PartialEq)]
enum Color {
    Red,
    Green,
    Blue,
}

#[tpd_make_enum]
#[derive(Debug, Clone, PartialEq)]
enum Size {
    Small,
    Medium,
    Large,
}

/// A struct using tpd_make_enum enums - no #[as_enum] needed!
#[tpd_make_partial]
#[derive(Debug)]
struct Config {
    color: Color,
    opt_color: Option<Color>,
    colors: Vec<Color>,
    opt_colors: Option<Vec<Color>>,
    size: Size,
}

#[test]
fn test_tpd_make_enum_happy_path() {
    let toml = r#"
        color = 'Red'
        opt_color = 'Green'
        colors = ['Blue', 'Red']
        opt_colors = ['Green', 'Blue']
        size = 'Medium'
    "#;

    let result: Result<Config, _> = deserialize::<PartialConfig, Config>(toml);
    dbg!(&result);
    assert!(result.is_ok());

    let config = result.unwrap();
    assert_eq!(config.color, Color::Red);
    assert_eq!(config.opt_color, Some(Color::Green));
    assert_eq!(config.colors, vec![Color::Blue, Color::Red]);
    assert_eq!(config.opt_colors, Some(vec![Color::Green, Color::Blue]));
    assert_eq!(config.size, Size::Medium);
}

#[test]
fn test_tpd_make_enum_optional_missing() {
    let toml = r#"
        color = 'Blue'
        # opt_color is missing
        colors = ['Red']
        # opt_colors is missing
        size = 'Large'
    "#;

    let result: Result<Config, _> = deserialize::<PartialConfig, Config>(toml);
    dbg!(&result);
    assert!(result.is_ok());

    let config = result.unwrap();
    assert_eq!(config.color, Color::Blue);
    assert_eq!(config.opt_color, None);
    assert_eq!(config.colors, vec![Color::Red]);
    assert_eq!(config.opt_colors, None);
    assert_eq!(config.size, Size::Large);
}

#[test]
fn test_tpd_make_enum_invalid_variant() {
    let toml = r#"
        color = 'Purple'
        colors = ['Red']
        size = 'Small'
    "#;

    let result: Result<Config, _> = deserialize::<PartialConfig, Config>(toml);
    dbg!(&result);

    match result {
        Err(DeserError::DeserFailure(errors, _)) => {
            assert!(errors.iter().any(|e| {
                e.inner
                    .spans
                    .iter()
                    .any(|s| s.msg.contains("Invalid enum variant"))
            }));
        }
        _ => panic!("Expected DeserFailure due to invalid enum variant"),
    }
}

#[test]
fn test_tpd_make_enum_wrong_type() {
    let toml = r#"
        color = 42
        colors = ['Red']
        size = 'Small'
    "#;

    let result: Result<Config, _> = deserialize::<PartialConfig, Config>(toml);
    dbg!(&result);

    match result {
        Err(DeserError::DeserFailure(errors, _)) => {
            assert!(errors
                .iter()
                .any(|e| { e.inner.spans.iter().any(|s| s.msg.contains("Wrong type")) }));
        }
        _ => panic!("Expected DeserFailure due to wrong type"),
    }
}

#[test]
fn test_tpd_make_enum_missing_required() {
    let toml = r#"
        # color is missing
        colors = ['Red']
        size = 'Small'
    "#;

    let result: Result<Config, _> = deserialize::<PartialConfig, Config>(toml);
    dbg!(&result);

    match result {
        Err(DeserError::DeserFailure(errors, _)) => {
            assert!(errors.iter().any(|e| {
                e.inner
                    .spans
                    .iter()
                    .any(|s| s.msg.contains("Missing required key"))
            }));
        }
        _ => panic!("Expected DeserFailure due to missing required field"),
    }
}

#[test]
fn test_tpd_make_enum_vec_with_invalid() {
    let toml = r#"
        color = 'Red'
        colors = ['Red', 'InvalidColor', 'Blue']
        size = 'Small'
    "#;

    let result: Result<Config, _> = deserialize::<PartialConfig, Config>(toml);
    dbg!(&result);

    match result {
        Err(e) => {
            insta::assert_snapshot!(e.pretty("test.toml"));
        }
        _ => panic!("Expected DeserFailure due to invalid enum in array"),
    }
}

/// Test that the error message includes helpful suggestions
#[test]
fn test_tpd_make_enum_error_suggestions() {
    let toml = r#"
        color = 'Rd'
        colors = ['Red']
        size = 'Small'
    "#;

    let result: Result<Config, _> = deserialize::<PartialConfig, Config>(toml);
    dbg!(&result);

    match result {
        Err(DeserError::DeserFailure(errors, _)) => {
            // Should suggest "Red" since "Rd" is close
            let has_suggestion = errors.iter().any(|e| {
                e.inner
                    .help
                    .as_ref()
                    .map(|h| h.contains("Red"))
                    .unwrap_or(false)
            });
            assert!(has_suggestion, "Expected error to suggest 'Red'");
        }
        _ => panic!("Expected DeserFailure"),
    }
}
