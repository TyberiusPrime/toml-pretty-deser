#![allow(clippy::map_unwrap_or)]
use toml_pretty_deser::prelude::*;

#[tpd]
#[derive(Debug, Clone, PartialEq)]
enum Color {
    Red,
    Green,
    Blue,
}

#[tpd]
#[derive(Debug, Clone, PartialEq)]
enum Size {
    Small,
    Medium,
    Large,
}

#[tpd]
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
    let toml = "
        color = 'Red'
        opt_color = 'Green'
        colors = ['Blue', 'Red']
        opt_colors = ['Green', 'Blue']
        size = 'Medium'
    ";

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
    let toml = "
        color = 'Blue'
        # opt_color is missing
        colors = ['Red']
        # opt_colors is missing
        size = 'Large'
    ";

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
    let toml = "
        color = 'Purple'
        colors = ['Red']
        size = 'Small'
    ";

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
    let toml = "
        color = 42
        colors = ['Red']
        size = 'Small'
    ";

    let result: Result<Config, _> = deserialize::<PartialConfig, Config>(toml);
    dbg!(&result);

    match result {
        Err(DeserError::DeserFailure(errors, _)) => {
            assert!(
                errors
                    .iter()
                    .any(|e| { e.inner.spans.iter().any(|s| s.msg.contains("Wrong type")) })
            );
        }
        _ => panic!("Expected DeserFailure due to wrong type"),
    }
}

#[test]
fn test_tpd_make_enum_missing_required() {
    let toml = "
        # color is missing
        colors = ['Red']
        size = 'Small'
    ";

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
    let toml = "
        color = 'Red'
        colors = ['Red', 'InvalidColor', 'Blue']
        size = 'Small'
    ";

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
    let toml = "
        color = 'Rd'
        colors = ['Red']
        size = 'Small'
    ";

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

// ============================================================================
// Tests for #[tpd_alias] on unit enum variants
// ============================================================================

/// Unit enum with aliases on variants
#[tpd]
#[derive(Debug, Clone, PartialEq)]
enum Status {
    #[tpd_alias("active", "enabled")]
    Active,
    #[tpd_alias("inactive", "disabled")]
    Inactive,
    #[tpd_alias("pending", "waiting")]
    Pending,
}

#[tpd]
#[derive(Debug)]
struct StatusConfig {
    status: Status,
    opt_status: Option<Status>,
    statuses: Vec<Status>,
}

#[test]
fn test_unit_enum_alias_primary_name() {
    // Test that the primary variant name still works
    let toml = "
        status = 'Active'
        opt_status = 'Inactive'
        statuses = ['Active', 'Inactive', 'Pending']
    ";

    let result: Result<StatusConfig, _> = deserialize::<PartialStatusConfig, StatusConfig>(toml);
    dbg!(&result);
    assert!(result.is_ok());

    let config = result.unwrap();
    assert_eq!(config.status, Status::Active);
    assert_eq!(config.opt_status, Some(Status::Inactive));
    assert_eq!(
        config.statuses,
        vec![Status::Active, Status::Inactive, Status::Pending]
    );
}

#[test]
fn test_unit_enum_alias_with_aliases() {
    // Test that aliases work
    let toml = "
        status = 'active'
        opt_status = 'disabled'
        statuses = ['enabled', 'inactive', 'waiting']
    ";

    let result: Result<StatusConfig, _> = deserialize::<PartialStatusConfig, StatusConfig>(toml);
    dbg!(&result);
    assert!(result.is_ok());

    let config = result.unwrap();
    assert_eq!(config.status, Status::Active);
    assert_eq!(config.opt_status, Some(Status::Inactive));
    assert_eq!(
        config.statuses,
        vec![Status::Active, Status::Inactive, Status::Pending]
    );
}

#[test]
fn test_unit_enum_alias_mixed() {
    // Test mixing primary names and aliases
    let toml = "
        status = 'Active'
        opt_status = 'disabled'
        statuses = ['Active', 'inactive', 'Pending']
    ";

    let result: Result<StatusConfig, _> = deserialize::<PartialStatusConfig, StatusConfig>(toml);
    dbg!(&result);
    assert!(result.is_ok());

    let config = result.unwrap();
    assert_eq!(config.status, Status::Active);
    assert_eq!(config.opt_status, Some(Status::Inactive));
    assert_eq!(
        config.statuses,
        vec![Status::Active, Status::Inactive, Status::Pending]
    );
}

#[test]
fn test_unit_enum_alias_invalid_still_errors() {
    // Test that invalid values still produce errors
    let toml = "
        status = 'unknown'
        statuses = ['Active']
    ";

    let result: Result<StatusConfig, _> = deserialize::<PartialStatusConfig, StatusConfig>(toml);
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
fn test_unit_enum_alias_suggestions_include_aliases() {
    // Test that error suggestions include aliases
    let toml = "
        status = 'activ'
        statuses = ['Active']
    ";

    let result: Result<StatusConfig, _> = deserialize::<PartialStatusConfig, StatusConfig>(toml);
    dbg!(&result);

    match result {
        Err(DeserError::DeserFailure(errors, _)) => {
            // Should suggest "Active" or "active" since "activ" is close
            let has_suggestion = errors.iter().any(|e| {
                e.inner
                    .help
                    .as_ref()
                    .map(|h| h.contains("Active") || h.contains("active"))
                    .unwrap_or(false)
            });
            assert!(
                has_suggestion,
                "Expected error to suggest 'Active' or 'active'"
            );
        }
        _ => panic!("Expected DeserFailure"),
    }
}

// ============================================================================
// Tests for enum with special character aliases like operators
// ============================================================================

/// Unit enum with operator symbols as aliases
#[tpd]
#[derive(Debug, Clone, PartialEq)]
enum ComparisonOperator {
    #[tpd_alias(">=")]
    GreaterThanOrEqual,
    #[tpd_alias("<=")]
    LessThanOrEqual,
    #[tpd_alias("==")]
    Equal,
    #[tpd_alias("!=")]
    NotEqual,
}

#[tpd]
#[derive(Debug)]
struct ComparisonConfig {
    operator: ComparisonOperator,
    opt_operator: Option<ComparisonOperator>,
    operators: Vec<ComparisonOperator>,
}

#[test]
fn test_unit_enum_with_special_char_aliases() {
    // Test that special character aliases work
    let toml = "
        operator = '>='
        opt_operator = '<='
        operators = ['>=', '==', '!=']
    ";

    let result: Result<ComparisonConfig, _> =
        deserialize::<PartialComparisonConfig, ComparisonConfig>(toml);
    dbg!(&result);
    assert!(result.is_ok());

    let config = result.unwrap();
    assert_eq!(config.operator, ComparisonOperator::GreaterThanOrEqual);
    assert_eq!(
        config.opt_operator,
        Some(ComparisonOperator::LessThanOrEqual)
    );
    assert_eq!(
        config.operators,
        vec![
            ComparisonOperator::GreaterThanOrEqual,
            ComparisonOperator::Equal,
            ComparisonOperator::NotEqual
        ]
    );
}

#[test]
fn test_unit_enum_special_char_primary_name() {
    // Test that primary variant names still work alongside special aliases
    let toml = "
        operator = 'GreaterThanOrEqual'
        opt_operator = 'Equal'
        operators = ['LessThanOrEqual', 'NotEqual']
    ";

    let result: Result<ComparisonConfig, _> =
        deserialize::<PartialComparisonConfig, ComparisonConfig>(toml);
    dbg!(&result);
    assert!(result.is_ok());

    let config = result.unwrap();
    assert_eq!(config.operator, ComparisonOperator::GreaterThanOrEqual);
    assert_eq!(config.opt_operator, Some(ComparisonOperator::Equal));
    assert_eq!(
        config.operators,
        vec![
            ComparisonOperator::LessThanOrEqual,
            ComparisonOperator::NotEqual
        ]
    );
}

#[test]
fn test_unit_enum_special_char_invalid() {
    // Test that invalid operators produce errors
    let toml = "
        operator = '>'
        operators = ['>=']
    ";

    let result: Result<ComparisonConfig, _> =
        deserialize::<PartialComparisonConfig, ComparisonConfig>(toml);
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
