use toml_pretty_deser::prelude::*;
#[tdp_make_enum]
#[derive(Debug, Clone)]
enum Example {
    One,
    TwoThree,
    Four,
}

#[make_partial]
#[derive(Debug)]
struct EnumOutput {
    #[alias(other_an_enum)]
    an_enum: Example,
    #[alias(other_opt_enum)]
    opt_enum: Option<Example>,
    #[alias(other_vec_enum)]
    vec_enum: Vec<Example>,
    #[alias(other_opt_vec_enum)]
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
        assert!(errors
            .iter()
            .any(|e| e.inner.spans[0].msg.contains("Invalid enum variant")));
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
        assert!(errors
            .iter()
            .any(|e| e.inner.spans[0].msg == "Missing required key: 'an_enum'."),);
    } else {
        panic!("Expected failure due to missing required enum field")
    }
}

#[make_partial]
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

    let result: Result<_, _> = deserialize::<PartialEnumSingleAllowed, EnumSingleAllowed>(toml);
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

    let result: Result<_, _> = deserialize::<PartialEnumSingleAllowed, EnumSingleAllowed>(toml);
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
