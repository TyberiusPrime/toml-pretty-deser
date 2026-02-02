use std::{cell::RefCell, rc::Rc};
use toml_pretty_deser::{
    AnnotatedError, AsEnum, DeserError, FromTomlTable, StringNamedEnum, ToConcrete, TomlHelper,
    TomlValue, deserialize, make_partial,
};

#[derive(Debug)]
#[make_partial]
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

// #[derive(Debug)]
// struct PartialOutput {
//     a_u8: TomlValue<u8>,
//     a_i64: TomlValue<i64>,
//     a_f64: TomlValue<f64>,
//     a_string: TomlValue<String>,
//     a_bool: TomlValue<bool>,
//     opt_a_u8: TomlValue<Option<u8>>,
//     opt_a_i64: TomlValue<Option<i64>>,
//     opt_a_f64: TomlValue<Option<f64>>,
//     opt_a_string: TomlValue<Option<String>>,
//     opt_a_bool: TomlValue<Option<bool>>,
//     verified_i16: TomlValue<i16>,
//     defaulted_i16: TomlValue<i16>,
// }

impl FromTomlTable<()> for PartialOutput {
    fn from_toml_table(helper: &mut TomlHelper<'_>, _partial: &()) -> Self {
        PartialOutput {
            a_u8: helper.get("a_u8"),
            a_i64: helper.get("a_i64"),
            a_f64: helper.get("a_f64"),
            a_string: helper.get("a_string"),
            a_bool: helper.get("a_bool"),
            opt_a_u8: helper.get("opt_a_u8"),
            opt_a_i64: helper.get("opt_a_i64"),
            opt_a_f64: helper.get("opt_a_f64"),
            opt_a_string: helper.get("opt_a_string"),
            opt_a_bool: helper.get("opt_a_bool"),
            verified_i16: helper.get("verified_i16").verify(|v: &i16| {
                if *v > 5 {
                    Ok(())
                } else {
                    Err("Too small".to_string())
                }
            }),
            defaulted_i16: helper.get("defaulted_i16").or_default(42),
        }
    }
}
//
// impl ToConcrete<Output> for PartialOutput {
//     fn collect_errors(&self, errors: &Rc<RefCell<Vec<AnnotatedError>>>) {
//         self.a_u8.register_error(errors);
//         self.a_i64.register_error(errors);
//         self.a_f64.register_error(errors);
//         self.a_string.register_error(errors);
//         self.a_bool.register_error(errors);
//         self.verified_i16.register_error(errors);
//         self.defaulted_i16.register_error(errors);
//     }
//
//     fn can_concrete(&self) -> bool {
//         self.a_u8.has_value()
//             && self.a_i64.has_value()
//             && self.a_f64.has_value()
//             && self.a_string.has_value()
//             && self.a_bool.has_value()
//             && self.verified_i16.has_value()
//             && self.defaulted_i16.has_value()
//     }
//
//     fn to_concrete(self) -> Option<Output> {
//         Some(Output {
//             a_u8: self.a_u8.unwrap(),
//             a_i64: self.a_i64.unwrap(),
//             a_f64: self.a_f64.unwrap(),
//             a_string: self.a_string.unwrap(),
//             a_bool: self.a_bool.unwrap(),
//             opt_a_u8: self.opt_a_u8.unwrap(),
//             opt_a_i64: self.opt_a_i64.unwrap(),
//             opt_a_f64: self.opt_a_f64.unwrap(),
//             opt_a_string: self.opt_a_string.unwrap(),
//             opt_a_bool: self.opt_a_bool.unwrap(),
//             verified_i16: self.verified_i16.unwrap(),
//             defaulted_i16: self.defaulted_i16.unwrap(),
//         })
//     }
// }

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
        assert_eq!(errors[0].inner.spans[0].msg, "Missing required key: a_u8");
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
        assert!(errors[0].inner.spans[0].msg.contains("Validation failed"));
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

impl FromTomlTable<()> for PartialComplexOutput {
    fn from_toml_table(helper: &mut TomlHelper<'_>, _partial: &()) -> Self {
        PartialComplexOutput {
            items: helper.get("items"),
            numbers: helper.get("numbers"),
            opt_items: helper.get("opt_items"),
        }
    }
}

#[test]
fn test_arrays_and_nested() {
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
                .any(|e| e.inner.spans[0].msg.contains("numbers"))
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

// Test struct with arrays and nested structs
#[make_partial]
#[derive(Debug)]
struct EnumOutput {
    an_enum: Example,
    opt_enum: Option<Example>,
    vec_enum: Vec<Example>,
    opt_vec_enum: Option<Vec<Example>>,
}

impl FromTomlTable<()> for PartialEnumOutput {
    fn from_toml_table(helper: &mut TomlHelper<'_>, _partial: &()) -> Self {
        PartialEnumOutput {
            an_enum: helper.get("an_enum").as_enum(),
            opt_enum: helper.get("opt_enum").as_enum(),
            vec_enum: helper.get("vec_enum").as_enum(),
            opt_vec_enum: helper.get("opt_vec_enum").as_enum(),
        }
    }
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
                .any(|e| e.inner.spans[0].msg.contains("an_enum"))
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

impl FromTomlTable<()> for PartialNested {
    fn from_toml_table(helper: &mut TomlHelper<'_>, _partial: &()) -> Self {
        PartialNested {
            name: helper.get("name"),
            value: helper.get("value"),
        }
    }
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

impl FromTomlTable<()> for PartialOuter {
    fn from_toml_table(helper: &mut TomlHelper<'_>, _partial: &()) -> Self {
        use toml_pretty_deser::AsNested;
        PartialOuter {
            inline_nested: helper.get("inline_nested").as_nested(&helper.errors),
            nested: helper.get("nested").as_nested(&helper.errors),
            opt_nested: helper.get("opt_nested").as_nested(&helper.errors),
            vec_nested: helper.get("vec_nested").as_nested(&helper.errors),
        }
    }
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
            &output.nested.as_ref().unwrap().name.as_ref().unwrap(),
            &&"a".to_string()
        );
        assert!(&output.nested.as_ref().unwrap().value.as_ref().is_none());
        assert_eq!(errors.len(), 1);
        assert_eq!(errors[0].inner.spans[0].msg, "Missing required key: value");
        assert!(output.opt_nested.as_ref().unwrap().is_none());
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

impl FromTomlTable<()> for PartialLevel2 {
    fn from_toml_table(helper: &mut TomlHelper<'_>, _partial: &()) -> Self {
        PartialLevel2 {
            data: helper.get("data"),
        }
    }
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

impl FromTomlTable<()> for PartialLevel1 {
    fn from_toml_table(helper: &mut TomlHelper<'_>, _partial: &()) -> Self {
        use toml_pretty_deser::AsNested;
        PartialLevel1 {
            name: helper.get("name"),
            level2: helper.get("level2").as_nested(&helper.errors),
            opt_level2: helper.get("opt_level2").as_nested(&helper.errors),
        }
    }
}

#[derive(Debug)]
#[make_partial]
struct Root {
    #[nested]
    level1: Level1,
    #[nested]
    opt_level1: Option<Level1>,
}

impl FromTomlTable<()> for PartialRoot {
    fn from_toml_table(helper: &mut TomlHelper<'_>, _partial: &()) -> Self {
        use toml_pretty_deser::AsNested;
        PartialRoot {
            level1: helper.get("level1").as_nested(&helper.errors),
            opt_level1: helper.get("opt_level1").as_nested(&helper.errors),
        }
    }
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
        assert!(partial.level1.as_ref().unwrap().level2.as_ref().is_none());
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
        assert!(
            errors
                .iter()
                .any(|e| e.inner.spans[0].msg.contains("Missing required key: data"))
        );
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
        assert!(
            errors
                .iter()
                .any(|e| e.inner.spans[0].msg.contains("Missing required key: data"))
        );
        assert_eq!(errors[0].pretty("test.toml"), 
           "  ╭─test.toml
  ┆
5 │         [level1.level2]
6 │             other = 2
  ┆             ──┬──    
  ┆               │      
  ┆               ╰─────── Unknown key: other
──╯
Hint: Did you mean: 'data'?
");
        assert_eq!(errors[1].pretty("test.toml"), 
            "  ╭─test.toml
  ┆

5 │         [level1.level2]
  ┆         ───────┬───────
  ┆                │       
  ┆                ╰──────── Missing required key: data
──╯
Hint: This key is required but was not found in the TOML document.
");
        // let pretty = errors[0].pretty("test.toml");
        // println!("Pretty error:\n{}", pretty);
    } else {
        panic!("Expected failure due to missing data field in level2");
    }
}
