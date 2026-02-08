use toml_pretty_deser::prelude::*;

// Test module for module-qualified types in tagged enums
mod inner_types {
    use toml_pretty_deser::prelude::*;

    #[tpd]
    #[derive(Debug, Clone)]
    pub struct ModuleInnerA {
        pub x: i32,
        pub y: u32,
    }

    #[tpd]
    #[derive(Debug, Clone)]
    pub struct ModuleInnerB {
        pub p: String,
        pub q: u32,
    }
}

#[tpd]
#[derive(Debug, Clone)]
struct InnerA {
    n: i32,
    o: u32,
}

#[tpd]
#[derive(Debug, Clone)]
struct InnerB {
    s: u32,
    t: u32,
}

#[tpd(tag = "kind", aliases = ["type"])]
#[derive(Debug)] //todo: why is this clone necessary
enum EitherOne {
    KindA(InnerA),
    KindB(InnerB),
}

#[tpd]
#[derive(Debug)]
struct OuterEither {
    #[tpd_nested]
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
    let result: Result<_, _> = deserialize_with_mode::<PartialOuterEither, OuterEither>(
        toml,
        FieldMatchMode::Exact,
        VecMode::Strict,
    );
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
    let result: Result<_, _> = deserialize_with_mode::<PartialOuterEither, OuterEither>(
        toml,
        FieldMatchMode::UpperLower,
        VecMode::Strict,
    );
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
    let result: Result<_, _> = deserialize_with_mode::<PartialOuterEither, OuterEither>(
        toml,
        FieldMatchMode::UpperLower,
        VecMode::Strict,
    );
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
    let result: Result<_, _> = deserialize_with_mode::<PartialOuterEither, OuterEither>(
        toml,
        FieldMatchMode::Exact,
        VecMode::Strict,
    );
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
    let result: Result<_, _> = deserialize_with_mode::<PartialOuterEither, OuterEither>(
        toml,
        FieldMatchMode::Exact,
        VecMode::Strict,
    );
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
    let result: Result<_, _> = deserialize_with_mode::<PartialOuterEither, OuterEither>(
        toml,
        FieldMatchMode::Exact,
        VecMode::Strict,
    );
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
        );
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
    let result: Result<_, _> = deserialize_with_mode::<PartialOuterEither, OuterEither>(
        toml,
        FieldMatchMode::Exact,
        VecMode::Strict,
    );
    dbg!(&result);
    if let Err(DeserError::DeserFailure(errors, _)) = result {
        assert!(errors.iter().any(|e| (e.inner.spans[0].msg
            == "Wrong type: integer, expected string")
            && (e.inner.help == Some("Available are: 'KindA' or 'KindB'".to_string()))));
        assert!(errors.iter().any(|e| {
            let pretty = e.pretty("test.toml");
            //println!("{}", pretty);
            // The span now points to the actual value `1` which is more precise
            pretty
                == "  ╭─test.toml
  ┆
2 │     [choice]
3 │         kind = 1
  ┆                ┬
  ┆                │
  ┆                ╰─ Wrong type: integer, expected string
──╯
Hint: Available are: 'KindA' or 'KindB'
"
        }));
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
    let result: Result<_, _> = deserialize_with_mode::<PartialOuterEither, OuterEither>(
        toml,
        FieldMatchMode::Exact,
        VecMode::Strict,
    );
    dbg!(&result);
    if let Err(DeserError::DeserFailure(errors, _)) = result {
        assert!(errors
            .iter()
            .any(|e| e.inner.spans[0].msg == "Missing required key: 'o'."));
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
    let result: Result<_, _> = deserialize_with_mode::<PartialOuterEither, OuterEither>(
        toml,
        FieldMatchMode::Exact,
        VecMode::Strict,
    );
    dbg!(&result);
    if let Err(DeserError::DeserFailure(errors, _)) = result {
        assert!(errors
            .iter()
            .any(|e| e.inner.spans[0].msg == "Unknown key."));
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
    let result: Result<_, _> = deserialize_with_mode::<PartialOuterEither, OuterEither>(
        toml,
        FieldMatchMode::Exact,
        VecMode::Strict,
    );
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
        assert!(errors
            .iter()
            .any(|e| e.inner.spans[0].msg == "Missing required key: 'n'."));
        assert!(errors
            .iter()
            .any(|e| e.inner.spans[0].msg == "Missing required key: 'o'."));
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
    let result: Result<_, _> = deserialize_with_mode::<PartialOuterEither, OuterEither>(
        toml,
        FieldMatchMode::AnyCase,
        VecMode::Strict,
    );
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
    let result: Result<_, _> = deserialize_with_mode::<PartialOuterEither, OuterEither>(
        toml,
        FieldMatchMode::Exact,
        VecMode::Strict,
    );
    dbg!(&result);
    if let Err(DeserError::DeserFailure(errors, _)) = result {
        assert!(errors
            .iter()
            .any(|e| e.inner.spans[0].msg == "Missing required key: 'choice'."));
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
    let result: Result<_, _> = deserialize_with_mode::<PartialOuterEither, OuterEither>(
        toml,
        FieldMatchMode::Exact,
        VecMode::Strict,
    );
    dbg!(&result);
    if let Err(DeserError::DeserFailure(errors, _)) = result {
        assert!(errors
            .iter()
            .any(|e| e.inner.spans[0].msg.contains("Wrong type")));
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
    let result: Result<_, _> = deserialize_with_mode::<PartialOuterEither, OuterEither>(
        toml,
        FieldMatchMode::Exact,
        VecMode::Strict,
    );
    dbg!(&result);
    if let Err(DeserError::DeserFailure(errors, output)) = result {
        assert!(
            errors.iter().any(
                |e| e.inner.spans[0].msg.contains("Multiple tag fields defined")
                    || e.inner.spans[0].msg.contains("Key/alias conflict")
            ),
            "expected error about multiple tag fields, got: {errors:?}",
        );
        assert!(matches!(
            output.choice.state,
            TomlValueState::MultiDefined { .. }
        ));
    } else {
        panic!("expected error when both kind and type are present");
    }
}

#[tpd]
#[derive(Debug)]
struct OuterMaybeEither {
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
        VecMode::Strict,
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
#[tpd]
#[derive(Debug)]
struct OuterManyTagged {
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
        VecMode::Strict,
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
        VecMode::SingleOk,
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

#[tpd]
#[derive(Debug)]
struct OuterManyTaggedAllowOne {
    choices: Vec<EitherOne>,
}

#[test]
fn test_many_either_one_allow_one_happy() {
    let toml = "
    [choices]
        kind = 'KindA'
        n = -5
        o = 1
    ";
    let result: Result<_, _> = deserialize_with_mode::<
        PartialOuterManyTaggedAllowOne,
        OuterManyTaggedAllowOne,
    >(toml, FieldMatchMode::Exact, VecMode::SingleOk);
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
    >(toml, FieldMatchMode::Exact, VecMode::SingleOk);
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

// Test for module-qualified types in tagged enums (module::Type)
#[tpd(tag = "variant")]
#[derive(Debug)]
enum ModuleQualifiedEnum {
    TypeA(inner_types::ModuleInnerA),
    TypeB(inner_types::ModuleInnerB),
}

#[tpd]
#[derive(Debug)]
struct OuterWithModuleQualifiedEnum {
    #[tpd_nested]
    item: ModuleQualifiedEnum,
}

#[test]
fn test_module_qualified_tagged_enum_type_a() {
    let toml = "
    [item]
        variant = 'TypeA'
        x = 42
        y = 100
    ";
    let result: Result<_, _> = deserialize_with_mode::<
        PartialOuterWithModuleQualifiedEnum,
        OuterWithModuleQualifiedEnum,
    >(toml, FieldMatchMode::Exact, VecMode::Strict);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        match output.item {
            ModuleQualifiedEnum::TypeA(inner) => {
                assert_eq!(inner.x, 42);
                assert_eq!(inner.y, 100);
            }
            ModuleQualifiedEnum::TypeB(_) => {
                panic!("expected TypeA variant");
            }
        }
    }
}

#[test]
fn test_module_qualified_tagged_enum_type_b() {
    let toml = "
    [item]
        variant = 'TypeB'
        p = 'hello'
        q = 200
    ";
    let result: Result<_, _> = deserialize_with_mode::<
        PartialOuterWithModuleQualifiedEnum,
        OuterWithModuleQualifiedEnum,
    >(toml, FieldMatchMode::Exact, VecMode::Strict);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        match output.item {
            ModuleQualifiedEnum::TypeA(_) => {
                panic!("expected TypeB variant");
            }
            ModuleQualifiedEnum::TypeB(inner) => {
                assert_eq!(inner.p, "hello");
                assert_eq!(inner.q, 200);
            }
        }
    }
}

// ============================================================================
// Tests for #[tpd_alias] on tagged enum variants
// ============================================================================

#[tpd]
#[derive(Debug, Clone)]
struct ConfigA {
    value_a: i32,
}

#[tpd]
#[derive(Debug, Clone)]
struct ConfigB {
    value_b: String,
}

/// Tagged enum with aliases on variants
#[tpd(tag = "type")]
#[derive(Debug)]
enum ConfigType {
    #[tpd_alias("config_a", "typeA")]
    ConfigA(ConfigA),
    #[tpd_alias("config_b", "typeB")]
    ConfigB(ConfigB),
}

#[tpd]
#[derive(Debug)]
struct OuterConfigType {
    #[tpd_nested]
    config: ConfigType,
}

#[test]
fn test_tagged_enum_alias_primary_name() {
    // Test that primary variant name still works
    let toml = "
    [config]
        type = 'ConfigA'
        value_a = 42
    ";
    let result: Result<_, _> = deserialize_with_mode::<PartialOuterConfigType, OuterConfigType>(
        toml,
        FieldMatchMode::Exact,
        VecMode::Strict,
    );
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        match output.config {
            ConfigType::ConfigA(inner) => {
                assert_eq!(inner.value_a, 42);
            }
            ConfigType::ConfigB(_) => {
                panic!("expected ConfigA variant");
            }
        }
    }
}

#[test]
fn test_tagged_enum_alias_with_alias() {
    // Test that alias works
    let toml = "
    [config]
        type = 'config_a'
        value_a = 100
    ";
    let result: Result<_, _> = deserialize_with_mode::<PartialOuterConfigType, OuterConfigType>(
        toml,
        FieldMatchMode::Exact,
        VecMode::Strict,
    );
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        match output.config {
            ConfigType::ConfigA(inner) => {
                assert_eq!(inner.value_a, 100);
            }
            ConfigType::ConfigB(_) => {
                panic!("expected ConfigA variant");
            }
        }
    }
}

#[test]
fn test_tagged_enum_alias_second_alias() {
    // Test that second alias works
    let toml = "
    [config]
        type = 'typeB'
        value_b = 'hello world'
    ";
    let result: Result<_, _> = deserialize_with_mode::<PartialOuterConfigType, OuterConfigType>(
        toml,
        FieldMatchMode::Exact,
        VecMode::Strict,
    );
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        match output.config {
            ConfigType::ConfigA(_) => {
                panic!("expected ConfigB variant");
            }
            ConfigType::ConfigB(inner) => {
                assert_eq!(inner.value_b, "hello world");
            }
        }
    }
}

#[test]
fn test_tagged_enum_alias_unknown_still_errors() {
    // Test that unknown variant still produces error with suggestions including aliases
    let toml = "
    [config]
        type = 'UnknownType'
        value_a = 1
    ";
    let result: Result<_, _> = deserialize_with_mode::<PartialOuterConfigType, OuterConfigType>(
        toml,
        FieldMatchMode::Exact,
        VecMode::Strict,
    );
    dbg!(&result);

    if let Err(DeserError::DeserFailure(errors, _)) = result {
        assert!(
            errors
                .iter()
                .any(|e| e.inner.spans[0].msg.contains("Unknown enum variant")),
            "Expected Unknown enum variant error"
        );
    } else {
        panic!("expected error for unknown variant");
    }
}

#[tpd]
#[derive(Debug)]
struct OuterManyConfigType {
    #[tpd_nested]
    configs: Vec<ConfigType>,
}

#[test]
fn test_tagged_enum_alias_in_vec_mixed() {
    // Test aliases work in Vec context with mixed primary names and aliases
    let toml = "
    [[configs]]
        type = 'ConfigA'
        value_a = 1

    [[configs]]
        type = 'config_b'
        value_b = 'alias used'

    [[configs]]
        type = 'typeA'
        value_a = 3
    ";
    let result: Result<_, _> = deserialize_with_mode::<
        PartialOuterManyConfigType,
        OuterManyConfigType,
    >(toml, FieldMatchMode::Exact, VecMode::Strict);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.configs.len(), 3);
        assert!(matches!(
            &output.configs[0],
            ConfigType::ConfigA(ConfigA { value_a: 1 })
        ));
        assert!(matches!(
            &output.configs[1],
            ConfigType::ConfigB(ConfigB { value_b }) if value_b == "alias used"
        ));
        assert!(matches!(
            &output.configs[2],
            ConfigType::ConfigA(ConfigA { value_a: 3 })
        ));
    }
}

// ============================================================================
// Tests for empty struct variants in tagged enums
// ============================================================================

/// Empty struct - has no fields
#[tpd]
#[derive(Debug, Clone, PartialEq)]
struct EmptyInner {}

/// Non-empty struct for contrast
#[tpd]
#[derive(Debug, Clone)]
struct NonEmptyInner {
    value: i32,
}

/// Tagged enum with an empty struct variant
#[tpd(tag = "kind")]
#[derive(Debug)]
enum MaybeEmpty {
    Empty(EmptyInner),
    NonEmpty(NonEmptyInner),
}

#[tpd]
#[derive(Debug)]
struct OuterMaybeEmpty {
    #[tpd_nested]
    item: MaybeEmpty,
}

#[test]
fn test_tagged_enum_empty_struct_variant() {
    let toml = "
    [item]
        kind = 'Empty'
    ";
    let result: Result<_, _> = deserialize_with_mode::<PartialOuterMaybeEmpty, OuterMaybeEmpty>(
        toml,
        FieldMatchMode::Exact,
        VecMode::Strict,
    );
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        match output.item {
            MaybeEmpty::Empty(inner) => {
                assert_eq!(inner, EmptyInner {});
            }
            MaybeEmpty::NonEmpty(_) => {
                panic!("expected Empty variant");
            }
        }
    }
}

#[test]
fn test_tagged_enum_non_empty_struct_variant() {
    let toml = "
    [item]
        kind = 'NonEmpty'
        value = 42
    ";
    let result: Result<_, _> = deserialize_with_mode::<PartialOuterMaybeEmpty, OuterMaybeEmpty>(
        toml,
        FieldMatchMode::Exact,
        VecMode::Strict,
    );
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        match output.item {
            MaybeEmpty::Empty(_) => {
                panic!("expected NonEmpty variant");
            }
            MaybeEmpty::NonEmpty(inner) => {
                assert_eq!(inner.value, 42);
            }
        }
    }
}

#[test]
fn test_tagged_enum_empty_struct_in_vec() {
    let toml = "
    [[items]]
        kind = 'Empty'

    [[items]]
        kind = 'NonEmpty'
        value = 100
    ";

    #[tpd]
    #[derive(Debug)]
    struct OuterManyMaybeEmpty {
        #[tpd_nested]
        items: Vec<MaybeEmpty>,
    }

    let result: Result<_, _> = deserialize_with_mode::<
        PartialOuterManyMaybeEmpty,
        OuterManyMaybeEmpty,
    >(toml, FieldMatchMode::Exact, VecMode::Strict);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.items.len(), 2);
        assert!(matches!(&output.items[0], MaybeEmpty::Empty(_)));
        assert!(matches!(
            &output.items[1],
            MaybeEmpty::NonEmpty(NonEmptyInner { value: 100 })
        ));
    }
}
