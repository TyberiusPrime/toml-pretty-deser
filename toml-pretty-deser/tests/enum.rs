#![allow(clippy::struct_field_names)]
use toml_pretty_deser::prelude::*;
#[tpd]
#[derive(Debug, Clone)]
enum Example {
    One,
    TwoThree,
    Four,
}

#[tpd]
#[derive(Debug)]
struct EnumOutput {
    #[tpd_alias(other_an_enum)]
    an_enum: Example,
    #[tpd_alias(other_opt_enum)]
    opt_enum: Option<Example>,
    #[tpd_alias(other_vec_enum)]
    vec_enum: Vec<Example>,
    #[tpd_alias(other_opt_vec_enum)]
    opt_vec_enum: Option<Vec<Example>>,
}

#[test]
fn test_enum_happy_path() {
    let toml = "
            an_enum = 'One'
            opt_enum = 'TwoThree'
            vec_enum = ['One', 'Four']
            opt_vec_enum = ['TwoThree', 'One']
        ";

    let result: Result<_, _> = deserialize::<PartialEnumOutput, EnumOutput>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert!(matches!(output.an_enum, Example::One));
        assert!(matches!(output.opt_enum, Some(Example::TwoThree)));
        assert_eq!(output.vec_enum.len(), 2);
        assert!(matches!(output.vec_enum[0], Example::One));
        assert!(matches!(output.vec_enum[1], Example::Four));
        assert!(output.opt_vec_enum.is_some());
        let opt_vec = output.opt_vec_enum.unwrap();
        assert_eq!(opt_vec.len(), 2);
        assert!(matches!(opt_vec[0], Example::TwoThree));
        assert!(matches!(opt_vec[1], Example::One));
    }
}

#[test]
fn test_enum_happy_path_alias() {
    let toml = "
            other_an_enum = 'One'
            other_opt_enum = 'TwoThree'
            other_vec_enum = ['One', 'Four']
            other_opt_vec_enum = ['TwoThree', 'One']
        ";

    let result: Result<_, _> = deserialize::<PartialEnumOutput, EnumOutput>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert!(matches!(output.an_enum, Example::One));
        assert!(matches!(output.opt_enum, Some(Example::TwoThree)));
        assert_eq!(output.vec_enum.len(), 2);
        assert!(matches!(output.vec_enum[0], Example::One));
        assert!(matches!(output.vec_enum[1], Example::Four));
        assert!(output.opt_vec_enum.is_some());
        let opt_vec = output.opt_vec_enum.unwrap();
        assert_eq!(opt_vec.len(), 2);
        assert!(matches!(opt_vec[0], Example::TwoThree));
        assert!(matches!(opt_vec[1], Example::One));
    }
}

#[test]
fn test_enum_optional_missing() {
    let toml = "
            an_enum = 'Four'
            # opt_enum is missing
            vec_enum = ['TwoThree']
            # opt_vec_enum is missing
        ";

    let result: Result<_, _> = deserialize::<PartialEnumOutput, EnumOutput>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert!(matches!(output.an_enum, Example::Four));
        assert!(output.opt_enum.is_none());
        assert_eq!(output.vec_enum.len(), 1);
        assert!(matches!(output.vec_enum[0], Example::TwoThree));
        assert!(output.opt_vec_enum.is_none());
    }
}

#[test]
fn test_enum_invalid_variant() {
    let toml = "
            an_enum = 'InvalidVariant'
            vec_enum = ['One']
        ";

    let result: Result<_, _> = deserialize::<PartialEnumOutput, EnumOutput>(toml);
    dbg!(&result);
    if let Err(DeserError::DeserFailure(errors, _)) = result {
        assert!(
            errors
                .iter()
                .any(|e| e.inner.spans[0].msg.contains("Invalid enum variant"))
        );
    } else {
        panic!("Expected failure due to invalid enum variant")
    }
}

#[test]
fn test_enum_missing_required() {
    let toml = "
            # an_enum is missing
            vec_enum = ['One']
        ";

    let result: Result<_, _> = deserialize::<PartialEnumOutput, EnumOutput>(toml);
    dbg!(&result);
    if let Err(DeserError::DeserFailure(errors, _)) = result {
        assert!(
            errors
                .iter()
                .any(|e| e.inner.spans[0].msg == "Missing required key: 'an_enum'."),
        );
    } else {
        panic!("Expected failure due to missing required enum field")
    }
}

#[tpd]
#[derive(Debug)]
struct EnumSingleAllowed {
    vec_enum: Vec<Example>,

    opt_vec_enum: Option<Vec<Example>>,
}

#[test]
fn test_enum_happy_single() {
    let toml = "
        vec_enum = 'One'
        opt_vec_enum = 'TwoThree'
    ";

    let result: Result<_, _> = deserialize_with_mode::<PartialEnumSingleAllowed, EnumSingleAllowed>(
        toml,
        FieldMatchMode::Exact,
        VecMode::SingleOk,
    );
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.vec_enum.len(), 1);
        assert!(matches!(output.vec_enum[0], Example::One));
        assert!(output.opt_vec_enum.is_some());
        let opt_vec = output.opt_vec_enum.unwrap();
        assert_eq!(opt_vec.len(), 1);
        assert!(matches!(opt_vec[0], Example::TwoThree));
    }
}

#[test]
fn test_enum_happy_single_left_off() {
    let toml = "
        vec_enum = 'One'
    ";

    let result: Result<_, _> = deserialize_with_mode::<PartialEnumSingleAllowed, EnumSingleAllowed>(
        toml,
        FieldMatchMode::Exact,
        VecMode::SingleOk,
    );
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.vec_enum.len(), 1);
        assert!(matches!(output.vec_enum[0], Example::One));
        assert!(output.opt_vec_enum.is_none());
    }
}

#[test]
fn test_enum_happy_path_multi() {
    let toml = "
            vec_enum = ['One', 'Four']
            opt_vec_enum = ['TwoThree', 'One']
        ";

    let result: Result<_, _> = deserialize::<PartialEnumSingleAllowed, EnumSingleAllowed>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.vec_enum.len(), 2);
        assert!(matches!(output.vec_enum[0], Example::One));
        assert!(matches!(output.vec_enum[1], Example::Four));
        assert!(output.opt_vec_enum.is_some());
        let opt_vec = output.opt_vec_enum.unwrap();
        assert_eq!(opt_vec.len(), 2);
        assert!(matches!(opt_vec[0], Example::TwoThree));
        assert!(matches!(opt_vec[1], Example::One));
    }
}

// =============================================================================
// Unit enum FieldMatchMode tests
// =============================================================================

#[tpd]
#[derive(Debug)]
struct EnumCaseInsensitive {
    my_enum: Example,
}

#[test]
fn test_unit_enum_upper_lower_mode() {
    // In UpperLower mode, different casing of enum variants should work
    let toml = "
        my_enum = 'one'
    ";

    let result: Result<_, _> = deserialize_with_mode::<
        PartialEnumCaseInsensitive,
        EnumCaseInsensitive,
    >(toml, FieldMatchMode::UpperLower, VecMode::Strict);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert!(matches!(output.my_enum, Example::One));
    }
}

#[test]
fn test_unit_enum_upper_lower_mode_uppercase() {
    // In UpperLower mode, uppercase variant should work
    let toml = "
        my_enum = 'ONE'
    ";

    let result: Result<_, _> = deserialize_with_mode::<
        PartialEnumCaseInsensitive,
        EnumCaseInsensitive,
    >(toml, FieldMatchMode::UpperLower, VecMode::Strict);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert!(matches!(output.my_enum, Example::One));
    }
}

#[test]
fn test_unit_enum_any_case_mode() {
    // In AnyCase mode, different case variants should match
    // TwoThree can be written as two_three, twoThree, TWOTHREE, etc.
    let toml = "
        my_enum = 'two_three'
    ";

    let result: Result<_, _> = deserialize_with_mode::<
        PartialEnumCaseInsensitive,
        EnumCaseInsensitive,
    >(toml, FieldMatchMode::AnyCase, VecMode::Strict);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert!(matches!(output.my_enum, Example::TwoThree));
    }
}

#[test]
fn test_unit_enum_any_case_mode_kebab() {
    // In AnyCase mode, kebab-case should also work
    let toml = "
        my_enum = 'two-three'
    ";

    let result: Result<_, _> = deserialize_with_mode::<
        PartialEnumCaseInsensitive,
        EnumCaseInsensitive,
    >(toml, FieldMatchMode::AnyCase, VecMode::Strict);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert!(matches!(output.my_enum, Example::TwoThree));
    }
}

#[test]
fn test_unit_enum_exact_mode_fails_wrong_case() {
    // In Exact mode (default), wrong case should fail
    let toml = "
        my_enum = 'one'
    ";

    let result: Result<_, _> = deserialize_with_mode::<
        PartialEnumCaseInsensitive,
        EnumCaseInsensitive,
    >(toml, FieldMatchMode::Exact, VecMode::Strict);
    dbg!(&result);
    assert!(result.is_err());
}

// =============================================================================
// Unit enum FieldMatchMode tests with aliases
// =============================================================================

/// Unit enum with aliases for testing FieldMatchMode interaction
#[tpd]
#[derive(Debug, Clone, PartialEq)]
enum StatusWithAlias {
    #[tpd_alias("on", "enabled")]
    Active,
    #[tpd_alias("off", "disabled")]
    Inactive,
}

#[tpd]
#[derive(Debug)]
struct StatusWithAliasConfig {
    status: StatusWithAlias,
}

#[test]
fn test_unit_enum_alias_with_anycase_mode() {
    // Test that aliases work with AnyCase mode
    // The alias is "on" (lowercase), but we use "ON" (uppercase)
    // With AnyCase mode, this should match
    let toml = "
        status = 'ON'
    ";

    let result: Result<_, _> = deserialize_with_mode::<
        PartialStatusWithAliasConfig,
        StatusWithAliasConfig,
    >(toml, FieldMatchMode::AnyCase, VecMode::Strict);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.status, StatusWithAlias::Active);
    }
}

#[test]
fn test_unit_enum_alias_with_upperlower_mode() {
    // Test that aliases work with UpperLower mode
    // The alias is "enabled" (lowercase), but we use "ENABLED" (uppercase)
    // With UpperLower mode, this should match
    let toml = "
        status = 'ENABLED'
    ";

    let result: Result<_, _> = deserialize_with_mode::<
        PartialStatusWithAliasConfig,
        StatusWithAliasConfig,
    >(toml, FieldMatchMode::UpperLower, VecMode::Strict);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.status, StatusWithAlias::Active);
    }
}

#[test]
fn test_unit_enum_alias_exact_mode_fails_wrong_case() {
    // Test that aliases with wrong case fail in Exact mode
    // The alias is "on" (lowercase), but we use "ON" (uppercase)
    // With Exact mode, this should fail
    let toml = "
        status = 'ON'
    ";

    let result: Result<_, _> = deserialize_with_mode::<
        PartialStatusWithAliasConfig,
        StatusWithAliasConfig,
    >(toml, FieldMatchMode::Exact, VecMode::Strict);
    dbg!(&result);
    assert!(result.is_err());
}

#[test]
fn test_unit_enum_alias_with_anycase_mode_kebab_style() {
    // Test that aliases work with AnyCase mode using kebab-case style
    // The variant is "Inactive", we use "in-active" (kebab style)
    // With AnyCase mode, this should match
    let toml = "
        status = 'In-Active'
    ";

    let result: Result<_, _> = deserialize_with_mode::<
        PartialStatusWithAliasConfig,
        StatusWithAliasConfig,
    >(toml, FieldMatchMode::AnyCase, VecMode::Strict);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.status, StatusWithAlias::Inactive);
    }
}
