use std::{cell::RefCell, rc::Rc};
use toml_pretty_deser::{
    AnnotatedError, AsOptionalTaggedEnum, AsTaggedEnum, AsVecTaggedEnum, DeserError,
    FieldMatchMode, FromTomlTable, StringNamedEnum, ToConcrete, TomlHelper, TomlValue,
    TomlValueState, VerifyFromToml, deserialize_with_mode, make_partial, make_partial_enum,
};

#[make_partial]
#[derive(Debug)]
struct InnerA {
    n: i32,
    o: u32,
}

#[make_partial]
#[derive(Debug)]
struct InnerB {
    s: u32,
    t: u32,
}

#[make_partial_enum] // creates PartialEitherOne { KindA(PartialInnerA, ...)}
#[derive(Debug)]
enum EitherOne {
    KindA(InnerA),
    KindB(InnerB),
}

#[make_partial]
#[derive(Debug)]
struct OuterEither {
    #[enum_tagged("kind")]
    #[alias("type")]
    choice: EitherOne,
}

#[test]
fn test_either_one_happy_a() {
    let toml = "
    [choice]
        kind = 'KindA'
        n = -5
        o = 1
    ";
    let result: Result<_, _> =
        deserialize_with_mode::<PartialOuterEither, OuterEither>(toml, FieldMatchMode::Exact);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        match output.choice {
            EitherOne::KindA(inner) => {
                assert_eq!(inner.n, -5);
                assert_eq!(inner.o, 1);
            }
            EitherOne::KindB(_) => {
                panic!("expected KindA variant");
            }
        }
    }
}

#[test]
fn test_either_one_happy_a_case_insensitive() {
    let toml = "
    [choice]
        KiNd = 'KindA'
        n = -5
        o = 1
    ";
    let result: Result<_, _> =
        deserialize_with_mode::<PartialOuterEither, OuterEither>(toml, FieldMatchMode::UpperLower);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        match output.choice {
            EitherOne::KindA(inner) => {
                assert_eq!(inner.n, -5);
                assert_eq!(inner.o, 1);
            }
            EitherOne::KindB(_) => {
                panic!("expected KindA variant");
            }
        }
    }
}

#[test]
fn test_either_one_happy_a_alias() {
    let toml = "
    [choice]
        type = 'KindA'
        n = -5
        o = 1
    ";
    let result: Result<_, _> =
        deserialize_with_mode::<PartialOuterEither, OuterEither>(toml, FieldMatchMode::UpperLower);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        match output.choice {
            EitherOne::KindA(inner) => {
                assert_eq!(inner.n, -5);
                assert_eq!(inner.o, 1);
            }
            EitherOne::KindB(_) => {
                panic!("expected KindA variant");
            }
        }
    }
}

#[test]
fn test_either_one_happy_b() {
    let toml = "
    choice = {
        kind = 'KindB',
        s = 5,
        t = 0
    }
    ";
    let result: Result<_, _> =
        deserialize_with_mode::<PartialOuterEither, OuterEither>(toml, FieldMatchMode::Exact);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        match output.choice {
            EitherOne::KindA(_) => {
                panic!("expected KindB variant");
            }
            EitherOne::KindB(inner) => {
                assert_eq!(inner.s, 5);
                assert_eq!(inner.t, 0);
            }
        }
    }
}

#[test]
fn test_either_one_unknown_kind() {
    let toml = "
    choice = {
        kind = 'KindX',
        s = 5,
        t = 0
    }
    ";
    let result: Result<_, _> =
        deserialize_with_mode::<PartialOuterEither, OuterEither>(toml, FieldMatchMode::Exact);
    dbg!(&result);
    if let Err(DeserError::DeserFailure(errors, _output)) = result {
        assert_eq!(errors.len(), 1);
        assert_eq!(errors[0].inner.spans[0].msg, "Unknown enum variant");
        assert_eq!(
            errors[0].inner.help,
            Some("Did you mean: 'KindA' or 'KindB'?".to_string())
        );
    } else {
        panic!();
    }
}

#[test]
fn test_either_one_missing_kind_tag() {
    let toml = "
    [choice]
        # kind is missing
        n = 3
        o = 7
    ";
    let result: Result<_, _> =
        deserialize_with_mode::<PartialOuterEither, OuterEither>(toml, FieldMatchMode::Exact);
    dbg!(&result);
    if let Err(DeserError::DeserFailure(errors, _)) = result {
        assert_eq!(errors.len(), 1);
        assert_eq!(
            errors[0].inner.spans[0].msg,
            "Missing required tag field: kind"
        );
        assert_eq!(
            errors[0].inner.help,
            Some("Available are: 'KindA' or 'KindB'".to_string())
        )
    } else {
        panic!("expected Missing required tag field: kind");
    }
}

#[test]
fn test_either_one_wrong_tag_type() {
    let toml = "
    [choice]
        kind = 1
        n = -1
        o = 2
    ";
    let result: Result<_, _> =
        deserialize_with_mode::<PartialOuterEither, OuterEither>(toml, FieldMatchMode::Exact);
    dbg!(&result);
    if let Err(DeserError::DeserFailure(errors, _)) = result {
        assert!(
            errors
                .iter()
                .any(|e| e.inner.spans[0].msg == "Wrong type: integer, expected string")
        );
        assert_eq!(
            errors[0].inner.help,
            Some("Available are: 'KindA' or 'KindB'".to_string())
        );
        let pretty = errors[0].pretty("test.toml");
        println!("{}", pretty);
        // The span now points to the actual value `1` which is more precise
        assert_eq!(
            pretty,
            "  ╭─test.toml
  ┆
2 │     [choice]
3 │         kind = 1
  ┆                ┬
  ┆                │
  ┆                ╰─ Wrong type: integer, expected string
──╯
Hint: Available are: 'KindA' or 'KindB'
"
        );
    } else {
        panic!("expected tag validation error");
    }
}

#[test]
fn test_either_one_missing_variant_field() {
    let toml = "
    [choice]
        kind = 'KindA'
        n = -5
        # o missing
    ";
    let result: Result<_, _> =
        deserialize_with_mode::<PartialOuterEither, OuterEither>(toml, FieldMatchMode::Exact);
    dbg!(&result);
    if let Err(DeserError::DeserFailure(errors, _)) = result {
        assert!(
            errors
                .iter()
                .any(|e| e.inner.spans[0].msg == "Missing required key: 'o'.")
        );
    } else {
        panic!("expected missing required key for variant field");
    }
}

#[test]
fn test_either_one_unknown_key_in_variant() {
    let toml = "
    choice = {
        kind = 'KindB',
        s = 5,
        t = 6,
        x = 9
    }
    ";
    let result: Result<_, _> =
        deserialize_with_mode::<PartialOuterEither, OuterEither>(toml, FieldMatchMode::Exact);
    dbg!(&result);
    if let Err(DeserError::DeserFailure(errors, _)) = result {
        assert!(
            errors
                .iter()
                .any(|e| e.inner.spans[0].msg == "Unknown key.")
        );
    } else {
        panic!("expected unknown key error inside variant");
    }
}

#[test]
fn test_either_one_fields_mismatch_variant() {
    // Tag says KindA, but fields for KindB are provided
    let toml = "
    choice = {
        kind = 'KindA',
        s = 1,
        t = 2
    }
    ";
    let result: Result<_, _> =
        deserialize_with_mode::<PartialOuterEither, OuterEither>(toml, FieldMatchMode::Exact);
    dbg!(&result);
    if let Err(DeserError::DeserFailure(errors, _)) = result {
        // Expect at least one unknown key (s or t) and one missing key (n or o)
        assert!(
            errors
                .iter()
                .filter(|e| e.inner.spans[0].msg.contains("Unknown key"))
                .count()
                == 2
        );
        assert!(
            errors
                .iter()
                .any(|e| e.inner.spans[0].msg == "Missing required key: 'n'.")
        );
        assert!(
            errors
                .iter()
                .any(|e| e.inner.spans[0].msg == "Missing required key: 'o'.")
        );
    } else {
        panic!("expected errors due to field/variant mismatch");
    }
}

#[test]
fn test_either_one_tag_key_is_case_flexible_in_anycase_mode() {
    // In AnyCase mode, tag field name IS case-flexible - 'Kind' matches 'kind'
    let toml = "
    choice = {
        Kind = 'KindB',
        s = 10,
        t = 11
    }
    ";
    let result: Result<_, _> =
        deserialize_with_mode::<PartialOuterEither, OuterEither>(toml, FieldMatchMode::AnyCase);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        match output.choice {
            EitherOne::KindA(_) => {
                panic!("expected KindB variant");
            }
            EitherOne::KindB(inner) => {
                assert_eq!(inner.s, 10);
                assert_eq!(inner.t, 11);
            }
        }
    }
}

#[test]
fn test_either_one_missing_choice_field() {
    let toml = "
        # missing choice entirely
    ";
    let result: Result<_, _> =
        deserialize_with_mode::<PartialOuterEither, OuterEither>(toml, FieldMatchMode::Exact);
    dbg!(&result);
    if let Err(DeserError::DeserFailure(errors, _)) = result {
        assert!(
            errors
                .iter()
                .any(|e| e.inner.spans[0].msg == "Missing required key: 'choice'.")
        );
    } else {
        panic!("expected missing required key.");
    }
}

#[test]
fn test_either_one_wrong_field_type_in_variant() {
    let toml = "
    choice = {
        kind = 'KindB',
        s = 'oops',
        t = 3
    }
    ";
    let result: Result<_, _> =
        deserialize_with_mode::<PartialOuterEither, OuterEither>(toml, FieldMatchMode::Exact);
    dbg!(&result);
    if let Err(DeserError::DeserFailure(errors, _)) = result {
        assert!(
            errors
                .iter()
                .any(|e| e.inner.spans[0].msg.contains("Wrong type"))
        );
    } else {
        panic!("expected wrong type error in variant field");
    }
}

#[test]
fn test_either_one_both_kind_and_type_present() {
    // Both 'kind' and 'type' are present - should error out
    let toml = "
    [choice]
        kind = 'KindA'
        type = 'KindB'
        n = -5
        o = 1
    ";
    let result: Result<_, _> =
        deserialize_with_mode::<PartialOuterEither, OuterEither>(toml, FieldMatchMode::Exact);
    dbg!(&result);
    if let Err(DeserError::DeserFailure(errors, output)) = result {
        assert!(
            errors.iter().any(
                |e| e.inner.spans[0].msg.contains("Multiple tag fields defined")
                    || e.inner.spans[0].msg.contains("Key/alias conflict")
            ),
            "expected error about multiple tag fields, got: {:?}",
            errors
        );
        assert!(matches!(
            output.choice.state,
            TomlValueState::MultiDefined { .. }
        ));
    } else {
        panic!("expected error when both kind and type are present");
    }
}

#[make_partial]
#[derive(Debug)]
struct OuterMaybeEither {
    #[enum_tagged("kind")]
    choice: Option<EitherOne>,
}

#[test]
fn test_maybe_either_one_happy_a() {
    let toml = "
    [choice]
        kind = 'KindA'
        n = -5
        o = 1
    ";
    let result: Result<_, _> = deserialize_with_mode::<PartialOuterMaybeEither, OuterMaybeEither>(
        toml,
        FieldMatchMode::Exact,
    );
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        match output.choice {
            Some(EitherOne::KindA(inner)) => {
                assert_eq!(inner.n, -5);
                assert_eq!(inner.o, 1);
            }
            _ => {
                panic!("expected KindA variant");
            }
        }
    }
}
#[make_partial]
#[derive(Debug)]
struct OuterManyTagged {
    #[enum_tagged("kind")]
    choices: Vec<EitherOne>,
}

#[test]
fn test_many_either_one_happy() {
    let toml = "
    [[choices]]
        kind = 'KindA'
        n = -5
        o = 1

    [[choices]]
        kind = 'KindA'
        n = 5
        o = 2
    [[choices]]
        kind = 'KindB'
        s = 23
        t = 10
    ";
    let result: Result<_, _> = deserialize_with_mode::<PartialOuterManyTagged, OuterManyTagged>(
        toml,
        FieldMatchMode::Exact,
    );
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.choices.len(), 3);
        assert!(matches!(
            output.choices[0],
            EitherOne::KindA(InnerA { n: -5, o: 1 })
        ));
        assert!(matches!(
            output.choices[1],
            EitherOne::KindA(InnerA { n: 5, o: 2 })
        ));
        assert!(matches!(
            output.choices[2],
            EitherOne::KindB(InnerB { s: 23, t: 10 })
        ));
    }
}

#[test]
fn test_many_either_empty() {
    let toml = "";
    let result: Result<_, _> = deserialize_with_mode::<PartialOuterManyTagged, OuterManyTagged>(
        toml,
        FieldMatchMode::Exact,
    );
    dbg!(&result);
    if let Err(DeserError::DeserFailure(errors, output)) = result {
        assert_eq!(errors.len(), 1);
        assert_eq!(
            errors[0].inner.spans[0].msg,
            "Missing required key: 'choices'."
        );
        assert!(output.choices.value.is_none());
    } else {
        panic!("expected error when both kind and type are present");
    }
}

#[make_partial]
#[derive(Debug)]
struct OuterManyTaggedAllowOne {
    #[tpd_allow_single]
    #[enum_tagged("kind")]
    choices: Vec<EitherOne>,
}

#[test]
fn test_many_either_one_allow_one_happy() {
    let toml = "
    [choices]
        kind = 'KindA',
        n = -5,
        o = 1,
    ";
    let result: Result<_, _> = deserialize_with_mode::<
        PartialOuterManyTaggedAllowOne,
        OuterManyTaggedAllowOne,
    >(toml, FieldMatchMode::Exact);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.choices.len(), 1);
        assert!(matches!(
            output.choices[0],
            EitherOne::KindA(InnerA { n: -5, o: 1 })
        ));
    }
}

#[test]
fn test_many_either_one_allow_one_happy_inline() {
    let toml = "
    choices = {
        kind = 'KindA',
        n = -5,
        o = 1,
    }
    ";
    let result: Result<_, _> = deserialize_with_mode::<
        PartialOuterManyTaggedAllowOne,
        OuterManyTaggedAllowOne,
    >(toml, FieldMatchMode::Exact);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.choices.len(), 1);
        assert!(matches!(
            output.choices[0],
            EitherOne::KindA(InnerA { n: -5, o: 1 })
        ));
    }
}
