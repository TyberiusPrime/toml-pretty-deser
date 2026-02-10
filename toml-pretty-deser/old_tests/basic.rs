use toml_pretty_deser::prelude::*;

#[tpd(partial = false)]
#[derive(Debug)]
struct Output {
    a_u8: u8,
    a_i64: i64,
    a_f64: f64,
    a_usize: usize,
    a_isize: isize,
    a_u64: u64,
    a_string: String,
    a_bool: bool,
    opt_a_u8: Option<u8>,
    opt_a_i64: Option<i64>,
    opt_a_f64: Option<f64>,
    opt_a_string: Option<String>,
    opt_a_bool: Option<bool>,
    verified_i16: i16,
    #[tpd_default_in_verify]
    defaulted_i16: i16,
}

impl VerifyFromToml for PartialOutput {
    fn verify(mut self, helper: &mut TomlHelper<'_>) -> Self {
        self.verified_i16 = self.verified_i16.verify(helper, |v: &i16| {
            if *v > 5 {
                Ok(())
            } else {
                Err((
                    "Too small".to_string(),
                    Some("Try a value larger than 5.".to_string()),
                ))
            }
        });
        self.defaulted_i16 = self.defaulted_i16.or_default(42);
        self
    }
}

#[test]
#[allow(clippy::float_cmp)]
fn test_happy_path() {
    let toml = "
            a_u8 = 255
            a_i64 = -123
            a_f64 = 6.724
            a_usize = 123
            a_isize = -12
            a_u64 = 3823174987
            a_string = 'Hello, World!'
            a_bool = true

            opt_a_u8 = 128
            opt_a_i64 = -456
            opt_a_f64 = 2.71
            opt_a_string = 'Optional String'
            opt_a_bool = false

            verified_i16 = 10
            defaulted_i16 = 100
        ";

    let result: Result<_, _> = deserialize::<PartialOutput, Output>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.a_u8, 255);
        assert_eq!(output.a_i64, -123);
        assert_eq!(output.a_f64, 6.724);
        assert_eq!(output.a_usize, 123);
        assert_eq!(output.a_isize, -12);
        assert_eq!(output.a_u64, 3823174987);
        assert_eq!(output.a_string, "Hello, World!");
        assert!(output.a_bool);
        assert_eq!(output.opt_a_u8, Some(128));
        assert_eq!(output.opt_a_i64, Some(-456));
        assert_eq!(output.opt_a_f64, Some(2.71));
        assert_eq!(output.opt_a_string, Some("Optional String".to_string()));
        assert_eq!(output.opt_a_bool, Some(false));
        assert_eq!(output.verified_i16, 10);
        assert_eq!(output.defaulted_i16, 100);
    }
}

#[test]
fn test_missing() {
    let toml = "
            # a_u8 = 255
            a_i64 = -123
            a_f64 = 6.724

            a_usize = 123
            a_isize = -12
            a_u64 = 3823174987
            a_string = 'Hello, World!'
            a_bool = true

            opt_a_u8 = 128
            opt_a_i64 = -456
            opt_a_f64 = 2.71
            opt_a_string = 'Optional String'
            opt_a_bool = false

            verified_i16 = 10
            defaulted_i16 = 100
        ";

    let result: Result<_, _> = deserialize::<PartialOutput, Output>(toml);
    dbg!(&result);
    if let Err(DeserError::DeserFailure(errors, output)) = result {
        assert_eq!(output.a_u8.into_option(), None);
        assert_eq!(output.a_i64.into_option(), Some(-123));
        assert_eq!(errors.len(), 1);
        assert_eq!(
            errors[0].inner.spans[0].msg,
            "Missing required key: 'a_u8'."
        );
    } else {
        panic!("wrong result")
    }
}

#[test]
fn test_optional_missing() {
    let toml = "
            a_u8 = 255
            a_i64 = -123
            a_f64 = 6.724
            a_usize = 123
            a_u64 = 3823174987
            a_string = 'Hello, World!'
            a_bool = true
            a_isize = -12

            # opt_a_u8 is missing
            # opt_a_i64 is missing
            # opt_a_f64 is missing
            # opt_a_string is missing
            # opt_a_bool is missing

            verified_i16 = 10
            # defaulted_i16 is missing - should use default
        ";

    let result: Result<_, _> = deserialize::<PartialOutput, Output>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.a_u8, 255);
        assert_eq!(output.opt_a_u8, None);
        assert_eq!(output.opt_a_i64, None);
        assert_eq!(output.opt_a_f64, None);
        assert_eq!(output.opt_a_string, None);
        assert_eq!(output.opt_a_bool, None);
        assert_eq!(output.defaulted_i16, 42); // default value
    }
}

#[test]
fn test_verify_failure() {
    let toml = "
            a_u8 = 255
            a_i64 = -123
            a_f64 = 6.724
            a_usize = 123
            a_u64 = 3823174987
            a_string = 'Hello, World!'
            a_bool = true
            a_isize = -12

            opt_a_u8 = 128
            opt_a_i64 = -456
            opt_a_f64 = 2.71
            opt_a_string = 'Optional String'
            opt_a_bool = false

            verified_i16 = 3
            defaulted_i16 = 100
        ";

    let result: Result<_, _> = deserialize::<PartialOutput, Output>(toml);
    dbg!(&result);
    if let Err(DeserError::DeserFailure(errors, _)) = result {
        assert_eq!(errors.len(), 1);
        assert_eq!(errors[0].inner.spans[0].msg, "Too small");
        assert_eq!(
            errors[0].inner.help,
            Some("Try a value larger than 5.".to_string())
        );
    } else {
        panic!("wrong result")
    }
}

#[test]
fn test_wrong_type() {
    let toml = "
            a_u8 = 'not a number'
            a_i64 = -123
            a_f64 = 6.724
            a_string = 'Hello, World!'
            a_bool = true

            opt_a_u8 = 128
            opt_a_i64 = -456
            opt_a_f64 = 2.71
            opt_a_string = 'Optional String'
            opt_a_bool = false

            verified_i16 = 10
            defaulted_i16 = 100
            a_isize = -12
            a_usize = 123
            a_u64 = 3823174987
        ";

    let result: Result<_, _> = deserialize::<PartialOutput, Output>(toml);
    dbg!(&result);
    if let Err(DeserError::DeserFailure(errors, _)) = result {
        assert_eq!(errors.len(), 1);
        assert!(errors[0].inner.spans[0].msg.contains("Wrong type"));
    } else {
        panic!("wrong result")
    }
}

#[test]
fn test_range_validation() {
    let toml = "
            a_u8 = 300
            a_i64 = -123
            a_f64 = 6.724
            a_string = 'Hello, World!'
            a_bool = true

            opt_a_u8 = 128
            opt_a_i64 = -456
            opt_a_f64 = 2.71
            opt_a_string = 'Optional String'
            opt_a_bool = false

            verified_i16 = 10
            defaulted_i16 = 100

            a_usize = 123
            a_isize = -12
            a_u64 = 3823174987
        ";

    let result: Result<_, _> = deserialize::<PartialOutput, Output>(toml);
    dbg!(&result);
    if let Err(DeserError::DeserFailure(errors, _)) = result {
        assert_eq!(errors.len(), 1);
        assert!(errors[0].inner.spans[0].msg.contains("out of range"));
    } else {
        panic!("wrong result")
    }
}

// Test struct with arrays and nested structs
#[tpd]
#[derive(Debug)]
struct ComplexOutput {
    items: Vec<String>,
    numbers: Vec<i64>,
    opt_items: Option<Vec<String>>,
}

#[test]
fn test_arrays() {
    let toml = "
            items = ['a', 'b', 'c']
            numbers = [1, 2, 3, 4, 5]
        ";

    let result: Result<_, _> = deserialize::<PartialComplexOutput, ComplexOutput>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.items, vec!["a", "b", "c"]);
        assert_eq!(output.numbers, vec![1, 2, 3, 4, 5]);
        assert_eq!(output.opt_items, None);
    }
}

#[test]
fn test_arrays_with_optional() {
    let toml = "
            items = ['x', 'y']
            numbers = [10, 20]
            opt_items = ['optional', 'items']
        ";

    let result: Result<_, _> = deserialize::<PartialComplexOutput, ComplexOutput>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.items, vec!["x", "y"]);
        assert_eq!(output.numbers, vec![10, 20]);
        assert_eq!(
            output.opt_items,
            Some(vec!["optional".to_string(), "items".to_string()])
        );
    }
}

#[test]
fn test_vec_missing() {
    let toml = "
            items = ['a']
            # missing numbers
        ";

    let result: Result<_, _> = deserialize::<PartialComplexOutput, ComplexOutput>(toml);
    dbg!(&result);
    if let Err(DeserError::DeserFailure(errors, _)) = result {
        assert!(
            errors
                .iter()
                .any(|e| e.inner.spans[0].msg == "Missing required key: 'numbers'.")
        );
    } else {
        panic!("Expected failure due to missing numbers field")
    }
}

#[test]
fn test_array_wrong_element_type() {
    let toml = "
            items = [1, 2, 3]  # wrong type - should be strings
            numbers = [1, 2, 3]
        ";

    let result: Result<_, _> = deserialize::<PartialComplexOutput, ComplexOutput>(toml);
    if let Err(err) = result {
        insta::assert_snapshot!(err.pretty("test.toml"));
    } else {
        panic!("Expected failure due to wrong array element type")
    }
}

// Test nested struct
#[derive(Debug)]
#[tpd]
struct Nested {
    name: String,
    value: i32,
}

#[derive(Debug)]
#[tpd]
struct Outer {
    #[tpd_nested]
    nested: Nested,
    #[tpd_nested]
    inline_nested: Nested,

    #[tpd_nested]
    opt_nested: Option<Nested>,

    #[tpd_nested]
    vec_nested: Vec<Nested>,
}

#[test]
fn test_nested_happy() {
    let toml = "
        inline_nested = {
            name = 'd',
            value = 4
        }
        [nested]
            name = 'a'
            value = 1

        [opt_nested]
            name = 'b'
            value = 2

        [[vec_nested]]
            name = 'c1'
            value = 31

        [[vec_nested]]
            name = 'c2'
            value = 32

        ";

    let result: Result<_, _> = deserialize::<PartialOuter, Outer>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.nested.name, "a");
        assert_eq!(output.nested.value, 1);
        assert_eq!(output.inline_nested.name, "d");
        assert_eq!(output.inline_nested.value, 4);
        assert_eq!(output.opt_nested.as_ref().unwrap().name, "b");
        assert_eq!(output.opt_nested.as_ref().unwrap().value, 2);
        assert_eq!(output.vec_nested.len(), 2);
        assert_eq!(output.vec_nested[0].name, "c1");
        assert_eq!(output.vec_nested[0].value, 31);
        assert_eq!(output.vec_nested[1].name, "c2");
        assert_eq!(output.vec_nested[1].value, 32);
    }
}

#[test]
fn test_nested_happy_half() {
    let toml = "
        [nested]
            name = 'a'
        ";

    let result: Result<_, _> = deserialize::<PartialOuter, Outer>(toml);
    dbg!(&result);
    if let Err(DeserError::DeserFailure(errors, output)) = result {
        assert_eq!(
            &output.nested.value.as_ref().unwrap().name.as_ref().unwrap(),
            &&"a".to_string()
        );
        assert!(
            &output
                .nested
                .value
                .as_ref()
                .unwrap()
                .value
                .as_ref()
                .is_none()
        );
        assert_eq!(errors.len(), 3);
        assert_eq!(
            errors[0].inner.spans[0].msg,
            "Missing required key: 'value'."
        );
        assert_eq!(
            errors[1].inner.spans[0].msg,
            "Missing required key: 'inline_nested'."
        );
        assert_eq!(
            errors[2].inner.spans[0].msg,
            "Missing required key: 'vec_nested'."
        );
        assert!(output.opt_nested.as_ref().unwrap().is_none());
        assert!(matches!(output.opt_nested.state, TomlValueState::Ok { .. }));

        assert!(matches!(
            output.inline_nested.state,
            TomlValueState::Missing { .. }
        ));
        assert!(matches!(
            output.vec_nested.state,
            TomlValueState::Missing { .. }
        ));
    } else {
        panic!();
    }
}

#[test]
fn test_nested_lower_failure() {
    let toml = "
        inline_nested = {
            name = 2,
            value = 4
        }
        [nested]
            name = 55
            value = 1

        [opt_nested]
            name = 55
            value = 2

        [[vec_nested]]
            name = 55
            value = 31

        [[vec_nested]]
            name = 55
            value = 32

        ";

    let result: Result<_, _> = deserialize::<PartialOuter, Outer>(toml);
    dbg!(&result);
    if let Err(DeserError::DeserFailure(_errors, output)) = result {
        assert_eq!(
            *output
                .opt_nested
                .value
                .as_ref()
                .unwrap()
                .as_ref()
                .unwrap()
                .value
                .as_ref()
                .unwrap(),
            2
        );

        assert!(matches!(output.opt_nested.state, TomlValueState::Nested));
        assert!(matches!(output.nested.state, TomlValueState::Nested));
        assert!(matches!(output.inline_nested.state, TomlValueState::Nested));
        assert!(matches!(output.vec_nested.state, TomlValueState::Nested));
    } else {
        panic!();
    }
}

// Test two-level nested structs
#[derive(Debug)]
#[tpd]
struct Level2 {
    data: String,
}

#[derive(Debug)]
#[tpd]
struct Level1 {
    name: String,
    #[tpd_nested]
    level2: Level2,
    #[tpd_nested]
    opt_level2: Option<Level2>,
}

#[derive(Debug)]
#[tpd]
struct Root {
    #[tpd_nested]
    level1: Level1,
    #[tpd_nested]
    opt_level1: Option<Level1>,
}

#[test]
fn test_two_level_nested_happy() {
    let toml = "
        [level1]
            name = 'level1_name'

        [level1.level2]
            data = 'level2_data'

        [opt_level1]
            name = 'opt_name'

        [opt_level1.level2]
            data = 'opt_level2_data'
        ";

    let result: Result<_, _> = deserialize::<PartialRoot, Root>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.level1.name, "level1_name");
        assert_eq!(output.level1.level2.data, "level2_data");
        assert!(output.opt_level1.is_some());
        assert_eq!(output.opt_level1.as_ref().unwrap().name, "opt_name");
        assert_eq!(
            output.opt_level1.as_ref().unwrap().level2.data,
            "opt_level2_data"
        );
        assert!(output.opt_level1.as_ref().unwrap().opt_level2.is_none());
    }
}

#[test]
fn test_two_level_nested_with_optional_nested() {
    let toml = "
        [level1]
            name = 'l1'

        [level1.level2]
            data = 'l2_data'

        [level1.opt_level2]
            data = 'opt_l2_data'
        ";

    let result: Result<_, _> = deserialize::<PartialRoot, Root>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.level1.name, "l1");
        assert_eq!(output.level1.level2.data, "l2_data");
        assert!(output.level1.opt_level2.is_some());
        assert_eq!(
            output.level1.opt_level2.as_ref().unwrap().data,
            "opt_l2_data"
        );
        assert!(output.opt_level1.is_none());
    }
}

#[test]
fn test_two_level_nested_inline_table() {
    let toml = "
        level1 = {
            name = 'inline_l1',
            level2 = {
                data = 'inline_l2'
            },
            opt_level2 = {
                data = 'inline_opt_l2'
            }
        }
        ";

    let result: Result<_, _> = deserialize::<PartialRoot, Root>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.level1.name, "inline_l1");
        assert_eq!(output.level1.level2.data, "inline_l2");
        assert!(output.level1.opt_level2.is_some());
        assert_eq!(
            output.level1.opt_level2.as_ref().unwrap().data,
            "inline_opt_l2"
        );
        assert!(output.opt_level1.is_none());
    }
}

#[test]
fn test_two_level_nested_inline_table_failure() {
    let toml = "
        level1 = {
            name = 'inline_l1',
            level2 = {
                data = 'inline_l2'
            },
            opt_level2 = {
                data = 23
            }
        }
        ";

    let result: Result<_, _> = deserialize::<PartialRoot, Root>(toml);
    dbg!(&result);
    if let Err(DeserError::DeserFailure(_errors, output)) = result {
        assert_eq!(
            output.level1.value.as_ref().unwrap().name.as_ref().unwrap(),
            "inline_l1"
        );
        assert_eq!(
            output
                .level1
                .value
                .as_ref()
                .unwrap()
                .level2
                .as_ref()
                .unwrap()
                .data
                .as_ref()
                .unwrap(),
            "inline_l2"
        );

        assert!(matches!(
            output.level1.value.as_ref().unwrap().opt_level2.state,
            TomlValueState::Nested
        ));
        assert!(
            output
                .level1
                .value
                .as_ref()
                .unwrap()
                .opt_level2
                .value
                .as_ref()
                .unwrap()
                .as_ref()
                .unwrap()
                .data
                .value
                .is_none()
        );
    } else {
        panic!("expected err")
    }
}

#[test]
fn test_two_level_nested_missing_inner() {
    let toml = "
        [level1]
            name = 'l1'
        # missing level1.level2
        ";

    let result: Result<_, _> = deserialize::<PartialRoot, Root>(toml);
    dbg!(&result);
    // Missing nested table returns StillIncomplete since the nested struct itself isn't present
    if let Err(DeserError::DeserFailure(errors, partial)) = result {
        assert!(
            partial
                .level1
                .value
                .as_ref()
                .unwrap()
                .level2
                .as_ref()
                .is_none()
        );
        assert_eq!(
            errors[0].inner.spans[0].msg,
            "Missing required key: 'level2'."
        );
    } else {
        panic!("Expected StillIncomplete due to missing level2 field");
    }
}

#[test]
fn test_two_level_nested_missing_inner_field() {
    let toml = "
        [level1]
            name = 'l1'

        [level1.level2]
            # missing data field
        ";

    let result: Result<_, _> = deserialize::<PartialRoot, Root>(toml);
    dbg!(&result);
    if let Err(DeserError::DeserFailure(errors, _)) = result {
        assert!(errors.iter().any(|e| {
            e.inner.spans[0]
                .msg
                .contains("Missing required key: 'data'.")
        }));
    } else {
        panic!("Expected failure due to missing data field in level2");
    }
}

#[test]
fn test_two_errors_pretty() {
    let toml = "
        [level1]
            name = 'l1'

        [level1.level2]
            other = 2
            # missing data field
        ";

    let result: Result<_, _> = deserialize::<PartialRoot, Root>(toml);
    dbg!(&result);
    if let Err(e) = result {
        insta::assert_snapshot!(e.pretty("test.toml"));
    } else {
        panic!("Expected failure due to missing data field in level2");
    }
}

// =============================================================================
// Tests for new FieldMatchMode and alias features
// =============================================================================

// Test struct with aliases
#[derive(Debug)]
#[tpd]
struct AliasedOutput {
    #[tpd_alias("alsoAName")]
    #[tpd_alias("alsoAName2")]
    my_name: String,
    #[tpd_alias("myValue")]
    my_value: i32,
    regular_field: String,
}

#[test]
fn test_alias_exact_mode() {
    // In Exact mode, aliases should work but different cases should not
    let toml = "
        alsoAName = 'using alias'
        myValue = 42
        regular_field = 'no alias'
    ";

    let result: Result<_, _> = deserialize::<PartialAliasedOutput, AliasedOutput>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.my_name, "using alias");
        assert_eq!(output.my_value, 42);
        assert_eq!(output.regular_field, "no alias");
    }
}
#[test]
fn test_alias_exact_mode2() {
    // In Exact mode, aliases should work but different cases should not
    let toml = "
        alsoAName2 = 'using alias no 2'
        myValue = 42
        regular_field = 'no alias'
    ";

    let result: Result<_, _> = deserialize::<PartialAliasedOutput, AliasedOutput>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.my_name, "using alias no 2");
        assert_eq!(output.my_value, 42);
        assert_eq!(output.regular_field, "no alias");
    }
}

#[test]
fn test_alias_exact_mode_failure() {
    // In Exact mode, different case should NOT work
    let toml = "
        my_Name = 'wrong case'
        myValue = 42
        regular_field = 'no alias'
    ";

    let result: Result<_, _> = deserialize::<PartialAliasedOutput, AliasedOutput>(toml);
    // Should fail because 'myname' is not 'myName' or 'my_name'
    assert!(result.is_err());
    if let Err(DeserError::DeserFailure(errors, _)) = result {
        // Should have errors about missing my_name and unknown myname
        assert!(!errors.is_empty());
    }
}

// Test struct for case-insensitive matching
#[derive(Debug)]
#[tpd]
struct CaseOutput {
    my_field: String,
    another_field: i32,
}

#[test]
fn test_upper_lower_mode() {
    // In UpperLower mode, different cases should work
    let toml = "
        MY_FIELD = 'upper case'
        Another_Field = 123
    ";

    let result: Result<_, _> = deserialize_with_mode::<PartialCaseOutput, CaseOutput>(
        toml,
        FieldMatchMode::UpperLower,
        VecMode::Strict,
    );
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.my_field, "upper case");
        assert_eq!(output.another_field, 123);
    }
}

#[test]
fn test_any_case_mode_snake() {
    // In AnyCase mode, snake_case should match
    let toml = "
        my_field = 'snake case'
        another_field = 456
    ";

    let result: Result<_, _> = deserialize_with_mode::<PartialCaseOutput, CaseOutput>(
        toml,
        FieldMatchMode::AnyCase,
        VecMode::Strict,
    );
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.my_field, "snake case");
        assert_eq!(output.another_field, 456);
    }
}

#[test]
fn test_any_case_mode_camel() {
    // In AnyCase mode, camelCase should match snake_case field
    let toml = "
        myField = 'camel case'
        anotherField = 789
    ";

    let result: Result<_, _> = deserialize_with_mode::<PartialCaseOutput, CaseOutput>(
        toml,
        FieldMatchMode::AnyCase,
        VecMode::Strict,
    );
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.my_field, "camel case");
        assert_eq!(output.another_field, 789);
    }
}

#[test]
fn test_any_case_mode_kebab() {
    // In AnyCase mode, kebab-case should match snake_case field
    let toml = "
        my-field = 'kebab case'
        another-field = 101
    ";

    let result: Result<_, _> = deserialize_with_mode::<PartialCaseOutput, CaseOutput>(
        toml,
        FieldMatchMode::AnyCase,
        VecMode::Strict,
    );
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.my_field, "kebab case");
        assert_eq!(output.another_field, 101);
    }
}

#[test]
fn test_any_case_mode_upper_camel() {
    // In AnyCase mode, UpperCamelCase should match snake_case field
    let toml = "
        MyField = 'upper camel'
        AnotherField = 202
    ";

    let result: Result<_, _> = deserialize_with_mode::<PartialCaseOutput, CaseOutput>(
        toml,
        FieldMatchMode::AnyCase,
        VecMode::Strict,
    );
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.my_field, "upper camel");
        assert_eq!(output.another_field, 202);
    }
}

#[test]
fn test_any_case_mode_shouty() {
    // In AnyCase mode, SHOUTY_SNAKE_CASE should match snake_case field
    let toml = "
        MY_FIELD = 'shouty'
        ANOTHER_FIELD = 303
    ";

    let result: Result<_, _> = deserialize_with_mode::<PartialCaseOutput, CaseOutput>(
        toml,
        FieldMatchMode::AnyCase,
        VecMode::Strict,
    );
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.my_field, "shouty");
        assert_eq!(output.another_field, 303);
    }
}

#[test]
fn test_any_case_mode_train() {
    // In AnyCase mode, Train-Case should match snake_case field
    let toml = "
        My-Field = 'train case'
        Another-Field = 404
    ";

    let result: Result<_, _> = deserialize_with_mode::<PartialCaseOutput, CaseOutput>(
        toml,
        FieldMatchMode::AnyCase,
        VecMode::Strict,
    );
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.my_field, "train case");
        assert_eq!(output.another_field, 404);
    }
}

// Test unknown key detection with case variants
#[test]
fn test_any_case_mode_unknown_key() {
    // In AnyCase mode, unknown keys should still be detected
    let toml = "
        my_field = 'correct'
        unknown_field = 'this should error'
    ";

    let result: Result<_, _> = deserialize_with_mode::<PartialCaseOutput, CaseOutput>(
        toml,
        FieldMatchMode::AnyCase,
        VecMode::Strict,
    );
    assert!(result.is_err());
    if let Err(DeserError::DeserFailure(errors, _)) = result {
        assert!(
            errors
                .iter()
                .any(|e| e.inner.spans[0].msg == "Unknown key.")
        );
    }
}

// Test aliases with AnyCase mode
#[test]
fn test_alias_with_any_case_mode() {
    let toml = "
        myName = 'alias match'
        myValue = 111
        regularField = 'case variant'
    ";

    let result: Result<_, _> = deserialize_with_mode::<PartialAliasedOutput, AliasedOutput>(
        toml,
        FieldMatchMode::AnyCase,
        VecMode::Strict,
    );
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.my_name, "alias match");
        assert_eq!(output.my_value, 111);
        assert_eq!(output.regular_field, "case variant");
    }
}

// Test nested struct with aliases and case variants
#[derive(Debug)]
#[tpd]
struct AliasedNested {
    #[tpd_alias("some_other_name")]
    nested_name: String,
}

#[derive(Debug)]
#[tpd]
struct OuterAliased {
    #[tpd_nested]
    nested: AliasedNested,
}

#[test]
fn test_nested_with_alias() {
    let toml = "
        [nested]
        some_other_name = 'aliased nested field'
    ";

    let result: Result<_, _> = deserialize::<PartialOuterAliased, OuterAliased>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.nested.nested_name, "aliased nested field");
    }
}

#[test]
fn test_nested_with_any_case_mode() {
    let toml = "
        [nested]
        NestedName = 'case variant in nested'
    ";

    let result: Result<_, _> = deserialize_with_mode::<PartialOuterAliased, OuterAliased>(
        toml,
        FieldMatchMode::AnyCase,
        VecMode::Strict,
    );
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.nested.nested_name, "case variant in nested");
    }
}

// Test complex case with mixed conventions
#[derive(Debug)]
#[tpd]
struct MixedCase {
    api_key: String,
    html_parser: String,
    get_http_response: i32,
}

#[test]
fn test_complex_mixed_case() {
    let toml = "
        API_KEY = 'shouty alias'
        HTMLPARSER = 'consecutive caps'
        getHTTPResponse = 42
    ";

    let result: Result<_, _> = deserialize_with_mode::<PartialMixedCase, MixedCase>(
        toml,
        FieldMatchMode::AnyCase,
        VecMode::Strict,
    );
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.api_key, "shouty alias");
        assert_eq!(output.html_parser, "consecutive caps");
        assert_eq!(output.get_http_response, 42);
    }
}

#[test]
fn test_complex_mixed_case_key_reused() {
    let toml = "
        API_KEY = 'shouty alias'
        htmlparser= '23'
        HTMLPARSER = 'consecutive caps'
        api-key = 24
        getHTTPResponse = 42
    ";

    let result: Result<_, _> = deserialize_with_mode::<PartialMixedCase, MixedCase>(
        toml,
        FieldMatchMode::AnyCase,
        VecMode::Strict,
    );
    //dbg!(&result);
    if let Err(DeserError::DeserFailure(_errors, output)) = &result {
        assert!(output.api_key.as_ref().is_none());
        assert!(output.html_parser.as_ref().is_none());
        assert_eq!(*output.get_http_response.as_ref().unwrap(), 42);
        insta::assert_snapshot!(result.unwrap_err().pretty("test.toml"));
    } else {
        panic!("should have been a DeserFailure");
    }
}

#[tpd]
#[derive(Debug)]
struct NestedWithVec {
    name: String,
    value: u8,
    entries: Vec<String>,
}

#[tpd]
#[derive(Debug)]
struct ArrayOfInlineTables {
    #[tpd_nested]
    inner: Vec<NestedWithVec>,
}

#[test]
fn test_inline_tables() {
    let toml = "
    inner = [
            {
                name = 'a',
                value = 1,
                entries = ['a','b']
            },
            {
                name = 'b',
                value = 2,
                entries = ['c','d', 'e']
            },
    ]
    ";

    let result: Result<_, _> = deserialize_with_mode::<
        PartialArrayOfInlineTables,
        ArrayOfInlineTables,
    >(toml, FieldMatchMode::Exact, VecMode::Strict);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.inner.len(), 2);
        assert_eq!(output.inner[0].name, "a");
        assert_eq!(output.inner[0].value, 1);
        assert_eq!(output.inner[0].entries.len(), 2);
        assert_eq!(output.inner[0].entries[0], "a");
        assert_eq!(output.inner[0].entries[1], "b");

        assert_eq!(output.inner[1].name, "b");
        assert_eq!(output.inner[1].value, 2);
        assert_eq!(output.inner[1].entries.len(), 3);
        assert_eq!(output.inner[1].entries[0], "c");
        assert_eq!(output.inner[1].entries[1], "d");
        assert_eq!(output.inner[1].entries[2], "e");
    }
}

#[allow(dead_code)]
#[tpd]
struct ShowOffTable {
    #[tpd_nested]
    something: Nested,
}

#[test]
fn showoff() {
    let toml = "
    [something]
        name = 'hello'
    ";
    let result = deserialize::<PartialShowOffTable, ShowOffTable>(toml);
    if let Err(DeserError::DeserFailure(errors, _)) = result {
        insta::assert_snapshot!(errors[0].pretty("test.toml"));
    } else {
        unreachable!("");
    }

    let toml = "
    something = {
        name = 'hello',
    }
    ";
    let result = deserialize::<PartialShowOffTable, ShowOffTable>(toml);
    if let Err(DeserError::DeserFailure(errors, _)) = result {
        println!("{}", errors[0].pretty("example-table.toml"));
        insta::assert_snapshot!(errors[0].pretty("test.toml"));
    } else {
        unreachable!("");
    }
}

#[allow(dead_code)]
#[tpd(partial = false)]
struct ShowOffTwoValueErrors {
    a: i64,
    b: i64,
    c: i64,
}

impl VerifyFromToml for PartialShowOffTwoValueErrors {
    fn verify(self, helper: &mut TomlHelper<'_>) -> Self
    where
        Self: Sized,
    {
        if let Some(a) = self.a.value
            && let Some(b) = self.b.value
            && let Some(c) = self.c.value
        {
            let sum = a + b + c;
            if sum != 99 {
                let spans = vec![
                    (
                        self.a.span(),
                        format!("a+b+c must add up to 100. Sum was {sum}."),
                    ),
                    (self.b.span(), "See a".to_string()),
                    (self.c.span(), "See c".to_string()),
                ];
                helper.add_err_by_spans(spans, "For example, set a = 33, b=66, c=0");
            }
        }
        self
    }
}

#[test]
fn test_showoff_value_errors() {
    let toml = "
        a = 5
        b = 10
        c = 3
";
    let result = deserialize::<PartialShowOffTwoValueErrors, ShowOffTwoValueErrors>(toml);
    if let Err(DeserError::DeserFailure(errors, _)) = result {
        insta::assert_snapshot!(errors[0].pretty("test.toml"));
    } else {
        unreachable!("");
    }
}

#[test]
fn test_parsing_error() {
    let toml = "
        a = 5,
        b = 10
        c = 3
";
    let result = deserialize::<PartialShowOffTwoValueErrors, ShowOffTwoValueErrors>(toml);
    assert!(result.is_err());
    if let Err(e) = result {
        insta::assert_snapshot!(e.pretty("test.toml"));
    }
}

#[tpd(partial = false)]
#[allow(dead_code)]
#[derive(Debug)]
struct DefaultConfig {
    #[tpd_default_in_verify]
    defaulted_i16: i16,
}

impl VerifyFromToml for PartialDefaultConfig {
    fn verify(self, _helper: &mut TomlHelper<'_>) -> Self {
        //not defaulting!
        self
    }
}
#[test]
#[should_panic = "The Partial was still incomplete, but no error was logged."]
fn test_default_but_not_set_in_verify() {
    let toml = "
    
";
    let result = deserialize::<PartialDefaultConfig, DefaultConfig>(toml);
    assert!(result.is_err());
    if let Err(e) = result {
        insta::assert_snapshot!(e.pretty("test.toml"));
    }
}

#[tpd(partial = false)]
#[allow(dead_code)]
#[derive(Debug)]
struct DefaultConfigDefaultWith {
    #[tpd_default_in_verify]
    defaulted_i16: i16,
}

impl VerifyFromToml for PartialDefaultConfigDefaultWith {
    fn verify(mut self, _helper: &mut TomlHelper<'_>) -> Self {
        //not defaulting!
        self.defaulted_i16 = self.defaulted_i16.or_default_with(|| 43);
        self
    }
}

#[test]
fn test_default_with() {
    let toml = "
    
";
    let result = deserialize::<PartialDefaultConfigDefaultWith, DefaultConfigDefaultWith>(toml);
    assert!(result.is_ok());
    if let Ok(config) = result {
        assert!(config.defaulted_i16 == 43);
    }
}

// =============================================================================
// Tests for Option<Vec<nested>>
// =============================================================================

#[derive(Debug)]
#[tpd]
struct NestedItem {
    name: String,
    value: i32,
}

#[derive(Debug)]
#[tpd]
struct OptionVecNestedOuter {
    #[tpd_nested]
    opt_items: Option<Vec<NestedItem>>,
    regular_field: String,
}

#[test]
fn test_option_vec_nested_present() {
    let toml = "
        regular_field = 'hello'
        [[opt_items]]
            name = 'item1'
            value = 10
        [[opt_items]]
            name = 'item2'
            value = 20
    ";

    let result: Result<_, _> =
        deserialize::<PartialOptionVecNestedOuter, OptionVecNestedOuter>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.regular_field, "hello");
        assert!(output.opt_items.is_some());
        let items = output.opt_items.unwrap();
        assert_eq!(items.len(), 2);
        assert_eq!(items[0].name, "item1");
        assert_eq!(items[0].value, 10);
        assert_eq!(items[1].name, "item2");
        assert_eq!(items[1].value, 20);
    }
}

#[test]
fn test_option_vec_nested_missing() {
    let toml = "
        regular_field = 'hello'
    ";

    let result: Result<_, _> =
        deserialize::<PartialOptionVecNestedOuter, OptionVecNestedOuter>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.regular_field, "hello");
        assert!(output.opt_items.is_none());
    }
}

#[test]
fn test_option_vec_nested_inline() {
    let toml = "
        regular_field = 'world'
        opt_items = [
            { name = 'a', value = 1 },
            { name = 'b', value = 2 },
            { name = 'c', value = 3 },
        ]
    ";

    let result: Result<_, _> =
        deserialize::<PartialOptionVecNestedOuter, OptionVecNestedOuter>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.regular_field, "world");
        assert!(output.opt_items.is_some());
        let items = output.opt_items.unwrap();
        assert_eq!(items.len(), 3);
        assert_eq!(items[0].name, "a");
        assert_eq!(items[2].name, "c");
    }
}

#[test]
fn test_option_vec_nested_empty() {
    let toml = "
        regular_field = 'test'
        opt_items = []
    ";

    let result: Result<_, _> =
        deserialize::<PartialOptionVecNestedOuter, OptionVecNestedOuter>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.regular_field, "test");
        assert!(output.opt_items.is_some());
        let items = output.opt_items.unwrap();
        assert_eq!(items.len(), 0);
    }
}

#[test]
fn test_option_vec_nested_inner_error() {
    let toml = "
        regular_field = 'test'
        [[opt_items]]
            name = 'item1'
            value = 10
        [[opt_items]]
            name = 123
            value = 20
    ";

    let result: Result<_, _> =
        deserialize::<PartialOptionVecNestedOuter, OptionVecNestedOuter>(toml);
    dbg!(&result);
    assert!(result.is_err());
    if let Err(DeserError::DeserFailure(errors, _)) = result {
        assert!(
            errors
                .iter()
                .any(|e| e.inner.spans[0].msg.contains("Wrong type"))
        );
    }
}

// =============================================================================
// Tests for integer values being accepted as f64
// =============================================================================

#[tpd]
#[derive(Debug)]
struct FloatFromIntOutput {
    a_f64: f64,
    opt_f64: Option<f64>,
    opt_f64_missing: Option<f64>,
}

#[test]
#[allow(clippy::float_cmp)]
fn test_f64_accepts_integer() {
    let toml = "
        a_f64 = 42
        opt_f64 = 100
    ";

    let result: Result<_, _> = deserialize::<PartialFloatFromIntOutput, FloatFromIntOutput>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.a_f64, 42.0);
        assert_eq!(output.opt_f64, Some(100.0));
        assert_eq!(output.opt_f64_missing, None);
    }
}

#[test]
#[allow(clippy::float_cmp)]
fn test_f64_accepts_negative_integer() {
    let toml = "
        a_f64 = -42
        opt_f64 = -100
    ";

    let result: Result<_, _> = deserialize::<PartialFloatFromIntOutput, FloatFromIntOutput>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.a_f64, -42.0);
        assert_eq!(output.opt_f64, Some(-100.0));
    }
}

#[test]
#[allow(clippy::float_cmp)]
fn test_f64_accepts_zero_integer() {
    let toml = "
        a_f64 = 0
        opt_f64 = 0
    ";

    let result: Result<_, _> = deserialize::<PartialFloatFromIntOutput, FloatFromIntOutput>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.a_f64, 0.0);
        assert_eq!(output.opt_f64, Some(0.0));
    }
}

#[test]
#[allow(clippy::float_cmp)]
fn test_f64_still_accepts_float() {
    // Ensure that explicit floats still work after the change
    let toml = "
        a_f64 = 3.14159
        opt_f64 = -2.71828
    ";

    let result: Result<_, _> = deserialize::<PartialFloatFromIntOutput, FloatFromIntOutput>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.a_f64, 3.14159);
        assert_eq!(output.opt_f64, Some(-2.71828));
    }
}

#[test]
#[allow(clippy::float_cmp)]
fn test_f64_mixed_int_and_float() {
    // Mix of integer and float values
    let toml = "
        a_f64 = 42
        opt_f64 = 3.14
    ";

    let result: Result<_, _> = deserialize::<PartialFloatFromIntOutput, FloatFromIntOutput>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.a_f64, 42.0);
        assert_eq!(output.opt_f64, Some(3.14));
    }
}

// =============================================================================
// Tests for VecMode::SingleOk with tpd_nested
// =============================================================================

#[tpd]
#[derive(Debug)]
struct NestedItemForVecMode {
    name: String,
    value: i32,
}

#[tpd]
#[derive(Debug)]
struct OuterWithNestedVec {
    #[tpd_nested]
    items: Vec<NestedItemForVecMode>,
}

#[test]
fn test_nested_vec_single_ok_mode_single_value() {
    // When using VecMode::SingleOk, a single nested table should be treated as [table]
    let toml = r#"
        [items]
        name = 'single'
        value = 42
    "#;

    let result = deserialize_with_mode::<PartialOuterWithNestedVec, OuterWithNestedVec>(
        toml,
        FieldMatchMode::Exact,
        VecMode::SingleOk,
    );
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.items.len(), 1);
        assert_eq!(output.items[0].name, "single");
        assert_eq!(output.items[0].value, 42);
    }
}

#[test]
fn test_nested_vec_single_ok_mode_array() {
    // VecMode::SingleOk should still work with arrays of tables
    let toml = r#"
        [[items]]
        name = 'first'
        value = 1
        [[items]]
        name = 'second'
        value = 2
    "#;

    let result = deserialize_with_mode::<PartialOuterWithNestedVec, OuterWithNestedVec>(
        toml,
        FieldMatchMode::Exact,
        VecMode::SingleOk,
    );
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.items.len(), 2);
        assert_eq!(output.items[0].name, "first");
        assert_eq!(output.items[1].name, "second");
    }
}

// =============================================================================
// Tests for tpd_nested with tpd_default enum with aliases
// =============================================================================

#[derive(Copy, Clone, Debug, Default, PartialEq, Eq)]
#[tpd]
pub enum CompressionFormat {
    #[tpd_alias("uncompressed", "raw")]
    #[default]
    Uncompressed,
    #[tpd_alias("gzip", "gz")]
    Gzip,
    #[tpd_alias("zstd", "zst")]
    Zstd,
}

#[tpd]
#[derive(Debug)]
struct InnerWithDefaultEnum {
    name: String,
    #[tpd_default]
    compression: CompressionFormat,
}

#[tpd]
#[derive(Debug)]
struct OuterWithOptionalNested {
    id: i32,
    #[tpd_nested]
    inner: Option<InnerWithDefaultEnum>,
}

#[test]
fn test_nested_default_enum_with_alias_primary() {
    // Test using primary enum variant name
    let toml = r#"
        id = 1
        [inner]
        name = 'test'
        compression = 'Gzip'
    "#;

    let result = deserialize::<PartialOuterWithOptionalNested, OuterWithOptionalNested>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.id, 1);
        assert!(output.inner.is_some());
        let inner = output.inner.unwrap();
        assert_eq!(inner.name, "test");
        assert_eq!(inner.compression, CompressionFormat::Gzip);
    }
}

#[test]
fn test_nested_default_enum_with_alias_lowercase() {
    // Test using alias (lowercase)
    let toml = r#"
        id = 1
        [inner]
        name = 'test'
        compression = 'gzip'
    "#;

    let result = deserialize::<PartialOuterWithOptionalNested, OuterWithOptionalNested>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        let inner = output.inner.unwrap();
        assert_eq!(inner.compression, CompressionFormat::Gzip);
    }
}

#[test]
fn test_nested_default_enum_with_alias_short() {
    // Test using short alias
    let toml = r#"
        id = 1
        [inner]
        name = 'test'
        compression = 'gz'
    "#;

    let result = deserialize::<PartialOuterWithOptionalNested, OuterWithOptionalNested>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        let inner = output.inner.unwrap();
        assert_eq!(inner.compression, CompressionFormat::Gzip);
    }
}

#[test]
fn test_nested_default_enum_missing_uses_default() {
    // Test that missing compression uses Default (Uncompressed)
    let toml = r#"
        id = 1
        [inner]
        name = 'test'
    "#;

    let result = deserialize::<PartialOuterWithOptionalNested, OuterWithOptionalNested>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        let inner = output.inner.unwrap();
        assert_eq!(inner.compression, CompressionFormat::Uncompressed);
    }
}

#[test]
fn test_nested_default_enum_zstd_alias() {
    // Test zstd alias
    let toml = r#"
        id = 1
        [inner]
        name = 'test'
        compression = 'zst'
    "#;

    let result = deserialize::<PartialOuterWithOptionalNested, OuterWithOptionalNested>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        let inner = output.inner.unwrap();
        assert_eq!(inner.compression, CompressionFormat::Zstd);
    }
}

// Test with FieldMatchMode::AnyCase - aliases should still work
#[test]
fn test_nested_default_enum_with_anycase_mode() {
    let toml = r#"
        id = 1
        [inner]
        name = 'test'
        Compression = 'gz'
    "#;

    let result = deserialize_with_mode::<PartialOuterWithOptionalNested, OuterWithOptionalNested>(
        toml,
        FieldMatchMode::AnyCase,
        VecMode::Strict,
    );
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        let inner = output.inner.unwrap();
        assert_eq!(inner.compression, CompressionFormat::Gzip);
    }
}

// Test with non-optional nested
#[tpd]
#[derive(Debug)]
#[allow(dead_code)]
struct OuterWithRequiredNested {
    id: i32,
    #[tpd_nested]
    inner: InnerWithDefaultEnum,
}

#[test]
fn test_required_nested_default_enum_with_alias() {
    let toml = r#"
        id = 1
        [inner]
        name = 'test'
        compression = 'zst'
    "#;

    let result = deserialize::<PartialOuterWithRequiredNested, OuterWithRequiredNested>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.inner.compression, CompressionFormat::Zstd);
    }
}

// Test with inline table
#[test]
fn test_nested_inline_default_enum_with_alias() {
    let toml = r#"
        id = 1
        inner = { name = 'test', compression = 'raw' }
    "#;

    let result = deserialize::<PartialOuterWithOptionalNested, OuterWithOptionalNested>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        let inner = output.inner.unwrap();
        assert_eq!(inner.compression, CompressionFormat::Uncompressed);
    }
}

// =============================================================================
// Tests for unit type fields (for non-field-specific errors)
// =============================================================================

/// Unit type fields are initialized with Ok((), 0..0) and don't read from TOML.
/// Users can set them to error states in verify() for non-field-specific errors.
#[tpd(partial = false)]
#[derive(Debug)]
struct ConfigWithUnitFields {
    name: String,
    value: i32,
    // Unit type fields for custom validation errors
    validation_passed: (),
    cross_field_check: (),
}

impl VerifyFromToml for PartialConfigWithUnitFields {
    fn verify(mut self, _helper: &mut TomlHelper<'_>) -> Self {
        // Custom cross-field validation
        if let Some(value) = self.value.value {
            if value < 0 {
                // Set unit field to error state for non-field-specific error
                self.validation_passed = TomlValue {
                    value: None,
                    state: TomlValueState::ValidationFailed {
                        span: 0..0,
                        message: "Value must be non-negative".to_string(),
                        help: Some("Please use a positive number".to_string()),
                    },
                };
            }
        }

        // Another cross-field check
        if let Some(name) = self.name.value.as_ref() && let Some(value) = self.value.value {
            if name == "special" && value != 42 {
                self.cross_field_check = TomlValue {
                    value: None,
                    state: TomlValueState::ValidationFailed {
                        span: 0..0,
                        message: "When name is 'special', value must be 42".to_string(),
                        help: None,
                    },
                };
            }
        }

        self
    }
}

#[test]
fn test_unit_fields_success() {
    // Valid configuration - no errors
    let toml = r#"
        name = "test"
        value = 10
    "#;

    let result = deserialize::<PartialConfigWithUnitFields, ConfigWithUnitFields>(toml);
    assert!(result.is_ok());
    if let Ok(config) = result {
        assert_eq!(config.name, "test");
        assert_eq!(config.value, 10);
        assert_eq!(config.validation_passed, ());
        assert_eq!(config.cross_field_check, ());
    }
}

#[test]
fn test_unit_fields_validation_error() {
    // Invalid configuration - negative value
    let toml = r#"
        name = "test"
        value = -5
    "#;

    let result = deserialize::<PartialConfigWithUnitFields, ConfigWithUnitFields>(toml);
    assert!(result.is_err());
    if let Err(DeserError::DeserFailure(errors, _)) = result {
        assert_eq!(errors.len(), 1);
        assert_eq!(errors[0].inner.spans[0].msg, "Value must be non-negative");
        assert_eq!(
            errors[0].inner.help,
            Some("Please use a positive number".to_string())
        );
    }
}

#[test]
fn test_unit_fields_cross_field_error() {
    // Invalid configuration - name is 'special' but value is not 42
    let toml = r#"
        name = "special"
        value = 10
    "#;

    let result = deserialize::<PartialConfigWithUnitFields, ConfigWithUnitFields>(toml);
    assert!(result.is_err());
    if let Err(DeserError::DeserFailure(errors, _)) = result {
        assert_eq!(errors.len(), 1);
        assert_eq!(
            errors[0].inner.spans[0].msg,
            "When name is 'special', value must be 42"
        );
    }
}

#[test]
fn test_unit_fields_multiple_errors() {
    // Invalid configuration - both errors should be reported
    let toml = r#"
        name = "special"
        value = -5
    "#;

    let result = deserialize::<PartialConfigWithUnitFields, ConfigWithUnitFields>(toml);
    assert!(result.is_err());
    if let Err(DeserError::DeserFailure(errors, _)) = result {
        assert_eq!(errors.len(), 2);
        // Check that both errors are present
        let error_messages: Vec<_> = errors
            .iter()
            .map(|e| e.inner.spans[0].msg.as_str())
            .collect();
        assert!(error_messages.contains(&"Value must be non-negative"));
        assert!(error_messages.contains(&"When name is 'special', value must be 42"));
    }
}

// =============================================================================
// Test that inner enum with #[tpd_default] and invalid value creates only ONE error
// =============================================================================

/// A unit enum for testing error behavior with #[tpd_default]
#[tpd]
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub enum Status {
    #[default]
    Active,
    Inactive,
    Pending,
}

#[tpd]
#[derive(Debug)]
struct ConfigWithDefaultEnum {
    name: String,
    #[tpd_default]
    status: Status,
}

#[test]
fn test_inner_enum_with_tpd_default_invalid_value_single_error() {
    // When an inner enum field with #[tpd_default] has an invalid value,
    // we should get exactly ONE error, not two.
    // The error should be about the invalid enum variant, not about a missing field.
    let toml = r#"
        name = "test"
        status = "InvalidStatus"
    "#;

    let result = deserialize::<PartialConfigWithDefaultEnum, ConfigWithDefaultEnum>(toml);
    dbg!(&result);

    if let Err(DeserError::DeserFailure(errors, _)) = result {
        // Should have exactly ONE error
        assert_eq!(
            errors.len(),
            1,
            "Expected exactly 1 error for invalid enum value, got {}: {:?}",
            errors.len(),
            errors
        );

        // The error should be about invalid enum variant
        assert!(
            errors[0].inner.spans[0].msg.contains("Invalid enum variant"),
            "Expected 'Invalid enum variant' error, got: {}",
            errors[0].inner.spans[0].msg
        );
    } else {
        panic!("Expected DeserFailure error, got: {:?}", result);
    }
}

#[test]
fn test_inner_enum_with_tpd_default_missing_uses_default() {
    // When the inner enum field with #[tpd_default] is missing, it should use the default.
    let toml = r#"
        name = "test"
    "#;

    let result = deserialize::<PartialConfigWithDefaultEnum, ConfigWithDefaultEnum>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.name, "test");
        assert_eq!(output.status, Status::Active); // Default value
    }
}

#[test]
fn test_inner_enum_with_tpd_default_valid_value() {
    // When the inner enum field with #[tpd_default] has a valid value, it should be used.
    let toml = r#"
        name = "test"
        status = "Pending"
    "#;

    let result = deserialize::<PartialConfigWithDefaultEnum, ConfigWithDefaultEnum>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.name, "test");
        assert_eq!(output.status, Status::Pending);
    }
}
