use std::{cell::RefCell, rc::Rc};
use toml_pretty_deser::{
    AnnotatedError, AsEnum, AsNested, DeserError, FieldMatchMode, FromTomlTable, StringNamedEnum,
    ToConcrete, TomlHelper, TomlValue, TomlValueState, VerifyFromToml, deserialize,
    deserialize_with_mode, make_partial,
};

#[make_partial(false)]
#[derive(Debug)]
struct Output {
    a_u8: u8,
    a_i64: i64,
    a_f64: f64,
    a_string: String,
    a_bool: bool,
    opt_a_u8: Option<u8>,
    opt_a_i64: Option<i64>,
    opt_a_f64: Option<f64>,
    opt_a_string: Option<String>,
    opt_a_bool: Option<bool>,
    verified_i16: i16,
    defaulted_i16: i16,
}

impl VerifyFromToml<()> for PartialOutput {
    fn verify(mut self, _helper: &mut TomlHelper<'_>, _partial: &()) -> Self {
        self.verified_i16 = self.verified_i16.verify(|v: &i16| {
            if *v > 5 {
                Ok(())
            } else {
                Err("Too small".to_string())
            }
        });
        self.defaulted_i16 = self.defaulted_i16.or_default(42);
        self
    }
}

#[test]
fn test_happy_path() {
    let toml = "
            a_u8 = 255
            a_i64 = -123
            a_f64 = 3.14
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
        assert_eq!(output.a_f64, 3.14);
        assert_eq!(output.a_string, "Hello, World!");
        assert_eq!(output.a_bool, true);
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
            a_f64 = 3.14
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
            a_f64 = 3.14
            a_string = 'Hello, World!'
            a_bool = true

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
fn test_validation_failure() {
    let toml = "
            a_u8 = 255
            a_i64 = -123
            a_f64 = 3.14
            a_string = 'Hello, World!'
            a_bool = true

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
    } else {
        panic!("wrong result")
    }
}

#[test]
fn test_wrong_type() {
    let toml = "
            a_u8 = 'not a number'
            a_i64 = -123
            a_f64 = 3.14
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
            a_f64 = 3.14
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
    if let Err(DeserError::DeserFailure(errors, _)) = result {
        assert_eq!(errors.len(), 1);
        assert!(errors[0].inner.spans[0].msg.contains("out of range"));
    } else {
        panic!("wrong result")
    }
}

// Test struct with arrays and nested structs
#[make_partial]
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
    dbg!(&result);
    if let Err(DeserError::DeserFailure(errors, _)) = result {
        assert!(errors.len() >= 1);
        // Should have errors about wrong type in array
    } else {
        panic!("Expected failure due to wrong array element type")
    }
}

#[derive(StringNamedEnum, Debug, Clone)]
enum Example {
    One,
    TwoThree,
    Four,
}

#[make_partial]
#[derive(Debug)]
struct EnumOutput {
    #[as_enum]
    an_enum: Example,
    #[as_enum]
    opt_enum: Option<Example>,
    #[as_enum]
    vec_enum: Vec<Example>,
    #[as_enum]
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

// Test nested struct
#[derive(Debug)]
#[make_partial]
struct Nested {
    name: String,
    value: i32,
}

#[derive(Debug)]
#[make_partial]
struct Outer {
    #[nested]
    nested: Nested,
    #[nested]
    inline_nested: Nested,

    #[nested]
    opt_nested: Option<Nested>,

    #[nested]
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
        assert_eq!(errors.len(), 1);
        assert_eq!(
            errors[0].inner.spans[0].msg,
            "Missing required key: 'value'."
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

        assert!(matches!(
            output.opt_nested.state,
            TomlValueState::ValidationFailed { .. }
        ));
        assert!(matches!(
            output.nested.state,
            TomlValueState::ValidationFailed { .. }
        ));
        assert!(matches!(
            output.inline_nested.state,
            TomlValueState::ValidationFailed { .. }
        ));
        assert!(matches!(
            output.vec_nested.state,
            TomlValueState::ValidationFailed { .. }
        ));
    } else {
        panic!();
    }
}

// Test two-level nested structs
#[derive(Debug)]
#[make_partial]
struct Level2 {
    data: String,
}

#[derive(Debug)]
#[make_partial]
struct Level1 {
    name: String,
    #[nested]
    level2: Level2,
    #[nested]
    opt_level2: Option<Level2>,
}

#[derive(Debug)]
#[make_partial]
struct Root {
    #[nested]
    level1: Level1,
    #[nested]
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
            TomlValueState::ValidationFailed { .. }
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
    if let Err(DeserError::StillIncomplete(_, partial)) = result {
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
    if let Err(DeserError::DeserFailure(errors, _)) = result {
        assert_eq!(errors.len(), 2);
        assert!(errors.iter().any(|e| {
            e.inner.spans[0]
                .msg
                .contains("Missing required key: 'data'.")
        }));
        assert_eq!(
            errors[0].pretty("test.toml"),
            "  ╭─test.toml\n  ┆\n5 │         [level1.level2]\n6 │             other = 2\n  ┆             ──┬──    \n  ┆               │      \n  ┆               ╰─────── Unknown key.\n──╯\nHint: Did you mean: 'data'?\n"
        );
        assert_eq!(
            errors[1].pretty("test.toml"),
            "  ╭─test.toml\n  ┆\n\n5 │         [level1.level2]\n  ┆         ───────┬───────\n  ┆                │       \n  ┆                ╰──────── Missing required key: 'data'.\n──╯\nHint: This key is required but was not found in the TOML document.\n"
        );
        // let pretty = errors[0].pretty("test.toml");
        // println!("Pretty error:\n{}", pretty);
    } else {
        panic!("Expected failure due to missing data field in level2");
    }
}

// =============================================================================
// Tests for new FieldMatchMode and alias features
// =============================================================================

// Test struct with aliases
#[derive(Debug)]
#[make_partial]
struct AliasedOutput {
    #[alias("alsoAName")]
    my_name: String,
    #[alias("myValue")]
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
        assert!(errors.len() >= 1);
    }
}

// Test struct for case-insensitive matching
#[derive(Debug)]
#[make_partial]
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

    let result: Result<_, _> =
        deserialize_with_mode::<PartialCaseOutput, CaseOutput>(toml, FieldMatchMode::UpperLower);
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

    let result: Result<_, _> =
        deserialize_with_mode::<PartialCaseOutput, CaseOutput>(toml, FieldMatchMode::AnyCase);
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

    let result: Result<_, _> =
        deserialize_with_mode::<PartialCaseOutput, CaseOutput>(toml, FieldMatchMode::AnyCase);
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

    let result: Result<_, _> =
        deserialize_with_mode::<PartialCaseOutput, CaseOutput>(toml, FieldMatchMode::AnyCase);
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

    let result: Result<_, _> =
        deserialize_with_mode::<PartialCaseOutput, CaseOutput>(toml, FieldMatchMode::AnyCase);
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

    let result: Result<_, _> =
        deserialize_with_mode::<PartialCaseOutput, CaseOutput>(toml, FieldMatchMode::AnyCase);
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

    let result: Result<_, _> =
        deserialize_with_mode::<PartialCaseOutput, CaseOutput>(toml, FieldMatchMode::AnyCase);
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

    let result: Result<_, _> =
        deserialize_with_mode::<PartialCaseOutput, CaseOutput>(toml, FieldMatchMode::AnyCase);
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

    let result: Result<_, _> =
        deserialize_with_mode::<PartialAliasedOutput, AliasedOutput>(toml, FieldMatchMode::AnyCase);
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
#[make_partial]
struct AliasedNested {
    #[alias("some_other_name")]
    nested_name: String,
}

#[derive(Debug)]
#[make_partial]
struct OuterAliased {
    #[nested]
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

    let result: Result<_, _> =
        deserialize_with_mode::<PartialOuterAliased, OuterAliased>(toml, FieldMatchMode::AnyCase);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.nested.nested_name, "case variant in nested");
    }
}

// Test complex case with mixed conventions
#[derive(Debug)]
#[make_partial]
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

    let result: Result<_, _> =
        deserialize_with_mode::<PartialMixedCase, MixedCase>(toml, FieldMatchMode::AnyCase);
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

    let result: Result<_, _> =
        deserialize_with_mode::<PartialMixedCase, MixedCase>(toml, FieldMatchMode::AnyCase);
    //dbg!(&result);
    if let Err(DeserError::DeserFailure(errors, output)) = result {
        assert!(output.api_key.as_ref().is_none());
        assert!(output.html_parser.as_ref().is_none());
        assert_eq!(*output.get_http_response.as_ref().unwrap(), 42);
        assert_eq!(errors.len(), 2);
        for e in &errors {
            println!("{}", e.pretty("test.toml"));
        }
        assert_eq!(
            errors[0].pretty("test.toml"),
            "  ╭─test.toml\n  ┆\n\n2 │         API_KEY = 'shouty alias'\n  ┆         ───┬───                 \n  ┆            │                    \n  ┆            ╰───────────────────── Key/alias conflict (defined multiple times)\n5 │         api-key = 24\n  ┆         ───┬───     \n  ┆            │        \n  ┆            ╰───────── Also defined here\n──╯\nHint: Use only one of the keys involved. Canonical is 'api_key'\n"
        );
        assert_eq!(
            errors[1].pretty("test.toml"),
            "  ╭─test.toml\n  ┆\n1 │ \n2 │         API_KEY = 'shouty alias'\n3 │         htmlparser= '23'\n  ┆         ─────┬────      \n  ┆              │          \n  ┆              ╰─────────── Key/alias conflict (defined multiple times)\n4 │         HTMLPARSER = 'consecutive caps'\n  ┆         ─────┬────                     \n  ┆              │                         \n  ┆              ╰────────────────────────── Also defined here\n──╯\nHint: Use only one of the keys involved. Canonical is 'html_parser'\n"
        );
    } else {
        panic!("should have been a DeserFailure");
    }
}

#[make_partial]
#[derive(Debug)]
struct NestedWithVec {
    name: String,
    value: u8,
    entries: Vec<String>,
}

#[make_partial]
#[derive(Debug)]
struct ArrayOfInlineTables {
    #[nested]
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
    >(toml, FieldMatchMode::Exact);
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
