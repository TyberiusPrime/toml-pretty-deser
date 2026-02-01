use std::{cell::RefCell, rc::Rc};
use toml_pretty_deser::{
    deserialize, make_partial, AnnotatedError, DeserError, FromTomlTable, ToConcrete, TomlHelper,
    TomlValue,
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

// Test struct with arrays and nested structs
#[make_partial]
#[derive(Debug)]
struct ComplexOutput {
    items: Vec<String>,
    numbers: Vec<i64>,
    //nested: Nested,
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
        assert!(errors
            .iter()
            .any(|e| e.inner.spans[0].msg.contains("numbers")));
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


#[derive(StringNamedEnum, Debug)]
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
    opt_vec_enum: Option<Vec<Example>>
}

impl FromTomlTable<()> for PartialEnumOutput {
    fn from_toml_table(helper: &mut TomlHelper<'_>, _partial: &()) -> Self {
        PartialEnumOutput {
            an_enum: helper.get("enum").as_enum(),
            opt_enum: helper.get("opt_enum").as_enum(),
            vec_enum: helper.get("vec_enum").as_enum(),
            opt_vec_enum: helper.get("opt_vec_enum").as_enum(),
        }
    }
}

