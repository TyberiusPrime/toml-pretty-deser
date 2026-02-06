use toml_pretty_deser::prelude::*;

// Test basic #[tpd_default] functionality
#[tpd]
#[derive(Debug)]
struct WithDefault {
    required: String,
    #[tpd_default]
    defaulted_u8: u8,
    #[tpd_default]
    defaulted_string: String,
    #[tpd_default]
    defaulted_bool: bool,
}

#[test]
fn test_tpd_default_when_missing() {
    let toml = r#"required = "hello""#;
    let result = deserialize::<PartialWithDefault, WithDefault>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.required, "hello");
        assert_eq!(output.defaulted_u8, 0); // Default::default() for u8
        assert_eq!(output.defaulted_string, ""); // Default::default() for String
        assert!(!output.defaulted_bool); // Default::default() for bool
    }
}

#[test]
fn test_tpd_default_when_provided() {
    let toml = r#"
        required = "hello"
        defaulted_u8 = 42
        defaulted_string = "custom"
        defaulted_bool = true
    "#;
    let result = deserialize::<PartialWithDefault, WithDefault>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.required, "hello");
        assert_eq!(output.defaulted_u8, 42);
        assert_eq!(output.defaulted_string, "custom");
        assert!(output.defaulted_bool);
    }
}

#[test]
fn test_tpd_default_partial_values() {
    // Some defaulted fields provided, some not
    let toml = r#"
        required = "hello"
        defaulted_u8 = 100
    "#;
    let result = deserialize::<PartialWithDefault, WithDefault>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.required, "hello");
        assert_eq!(output.defaulted_u8, 100);
        assert_eq!(output.defaulted_string, ""); // defaulted
        assert!(!output.defaulted_bool); // defaulted
    }
}

// Test that #[tpd_default] doesn't affect required field errors
#[test]
fn test_tpd_default_required_field_still_required() {
    let toml = r#"
        defaulted_u8 = 42
    "#;
    let result = deserialize::<PartialWithDefault, WithDefault>(toml);
    dbg!(&result);
    assert!(result.is_err()); // required field is missing
}

// Test with a type that has a non-trivial Default implementation
#[derive(Debug, Default, PartialEq)]
struct CustomDefault {
    value: i32,
}

impl FromTomlItem for CustomDefault {
    fn from_toml_item(
        item: &toml_edit::Item,
        parent_span: std::ops::Range<usize>,
        _col: &TomlCollector,
    ) -> TomlValue<Self> {
        match item {
            toml_edit::Item::Value(toml_edit::Value::Integer(formatted)) => {
                let value = *formatted.value() as i32;
                TomlValue::new_ok(
                    CustomDefault { value },
                    formatted.span().unwrap_or(parent_span),
                )
            }
            toml_edit::Item::None => TomlValue::new_empty_missing(parent_span),
            other => TomlValue::new_wrong_type(other, parent_span, "integer"),
        }
    }
}

#[tpd]
#[derive(Debug)]
struct WithCustomDefault {
    name: String,
    #[tpd_default]
    custom: CustomDefault,
}

#[test]
fn test_tpd_default_custom_type() {
    let toml = r#"name = "test""#;
    let result = deserialize::<PartialWithCustomDefault, WithCustomDefault>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.name, "test");
        assert_eq!(output.custom, CustomDefault::default());
        assert_eq!(output.custom.value, 0);
    }
}

#[test]
fn test_tpd_default_custom_type_provided() {
    let toml = r#"
        name = "test"
        custom = 99
    "#;
    let result = deserialize::<PartialWithCustomDefault, WithCustomDefault>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.name, "test");
        assert_eq!(output.custom.value, 99);
    }
}

// Test combining #[tpd_default] with #[tpd_alias]
#[tpd]
#[allow(dead_code)]
#[derive(Debug)]
struct WithDefaultAndAlias {
    name: String,
    #[tpd_default]
    #[tpd_alias("count", "num")]
    number: u32,
}

#[test]
fn test_tpd_default_with_alias() {
    // Using alias
    let toml = r#"
        name = "test"
        count = 5
    "#;
    let result = deserialize::<PartialWithDefaultAndAlias, WithDefaultAndAlias>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.number, 5);
    }
}

#[test]
fn test_tpd_default_with_alias_missing() {
    // No value, should default
    let toml = r#"name = "test""#;
    let result = deserialize::<PartialWithDefaultAndAlias, WithDefaultAndAlias>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.number, 0);
    }
}

// Test all fields being #[tpd_default]
#[tpd]
#[derive(Debug)]
struct AllDefaults {
    #[tpd_default]
    a: u8,
    #[tpd_default]
    b: String,
    #[tpd_default]
    c: bool,
}

#[test]
fn test_tpd_default_all_fields() {
    let toml = "";
    let result = deserialize::<PartialAllDefaults, AllDefaults>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.a, 0);
        assert_eq!(output.b, "");
        assert!(!output.c);
    }
}

// Test Vec with Default
#[tpd]
#[derive(Debug)]
struct WithDefaultVec {
    name: String,
    #[tpd_default]
    items: Vec<u32>,
}

#[test]
fn test_tpd_default_vec_when_missing() {
    let toml = r#"name = "test""#;
    let result = deserialize::<PartialWithDefaultVec, WithDefaultVec>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.name, "test");
        assert!(output.items.is_empty());
    }
}

#[test]
fn test_tpd_default_vec_when_provided() {
    let toml = r#"
        name = "test"
        items = [1, 2, 3]
    "#;
    let result = deserialize::<PartialWithDefaultVec, WithDefaultVec>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.name, "test");
        assert_eq!(output.items, vec![1, 2, 3]);
    }
}
