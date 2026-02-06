#![allow(dead_code)]
use toml_pretty_deser::prelude::*;

#[tpd]
#[derive(Debug, PartialEq, Eq)]
struct MyUnit(String);

#[tpd]
#[derive(Debug, PartialEq, Eq)]
struct MyInt(i8);

#[tpd]
#[derive(Debug)]
struct WithUnit {
    a: MyUnit,
    b: MyInt,
}

#[test]
#[allow(clippy::float_cmp)]
fn test_happy_path() {
    let toml = "
        a = 'hello'
         b = 120
        ";

    let result: Result<_, _> = deserialize::<PartialWithUnit, WithUnit>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.a, MyUnit("hello".to_string()));
        assert_eq!(output.b, MyInt(120));
    }
}

#[tpd]
#[derive(Debug)]
struct ReferenceTest {
    segments: Vec<String>,
    #[tpd_adapt_in_verify]
    inner: SegmentIndex,
}

#[derive(Debug)]
struct SegmentIndex(usize);

impl VerifyFromToml for PartialReferenceTest {
    fn verify(mut self, helper: &mut TomlHelper<'_>) -> Self {
        // Use the generated tpd_get_inner method to read the field as String
        let str_inner: TomlValue<String> = self.tpd_get_inner(helper, true);
        match str_inner.as_ref() {
            Some(segment_name) => {
                if let Some(segments) = self.segments.as_ref() {
                    let index = segments.iter().position(|s| s == segment_name);
                    match index {
                        Some(index) => {
                            self.inner = TomlValue::new_ok(SegmentIndex(index), str_inner.span())
                        }
                        None => {
                            self.inner = TomlValue::new_validation_failed(
                                str_inner.span(),
                                "Not a valid segment".to_string(),
                                Some("use one of the defined ones".to_string()),
                            );
                            self.inner.register_error(&helper.col.errors);
                        }
                    }
                } else {
                    self.inner = TomlValue::new_nested();
                }
            }
            None => {
                self.inner = str_inner.convert_failed_type();
            }
        }

        self
    }
}

#[test]
fn test_segment_happy() {
    let toml = "
        segments = ['a','b']
        inner = 'b'
        ";

    let result: Result<_, _> = deserialize::<PartialReferenceTest, ReferenceTest>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.inner.0, 1);
        assert_eq!(output.segments.len(), 2);
    }
}

#[test]
fn test_segment_invalid_reference() {
    let toml = "
        segments = ['a','b']
        inner = 'c'
        ";

    let result: Result<_, _> = deserialize::<PartialReferenceTest, ReferenceTest>(toml);
    dbg!(&result);
    assert!(result.is_err());
    if let Err(e) = result {
        let err_str = format!("{:?}", e);
        assert!(err_str.contains("Not a valid segment"));
    }
}

#[test]
fn test_segment_wrong_type() {
    let toml = "
        segments = ['a','b']
        inner = 123
        ";

    let result: Result<_, _> = deserialize::<PartialReferenceTest, ReferenceTest>(toml);
    dbg!(&result);
    assert!(result.is_err());
}

#[test]
fn test_segment_missing() {
    let toml = "
        segments = ['a','b']
        ";

    let result: Result<_, _> = deserialize::<PartialReferenceTest, ReferenceTest>(toml);
    dbg!(&result);
    assert!(result.is_err());
    if let Err(e) = result {
        let err_str = format!("{:?}", e);
        assert!(err_str.contains("inner"));
    }
}

// Test with aliases
#[tpd]
#[derive(Debug)]
struct AliasedAdaptTest {
    items: Vec<String>,
    #[tpd_adapt_in_verify]
    #[tpd_alias("item_ref", "ref")]
    selected: ItemIndex,
}

#[derive(Debug)]
struct ItemIndex(usize);

impl VerifyFromToml for PartialAliasedAdaptTest {
    fn verify(mut self, helper: &mut TomlHelper<'_>) -> Self {
        let str_selected: TomlValue<String> = self.tpd_get_selected(helper, true);
        match str_selected.as_ref() {
            Some(item_name) => {
                if let Some(items) = self.items.as_ref() {
                    match items.iter().position(|s| s == item_name) {
                        Some(idx) => {
                            self.selected = TomlValue::new_ok(ItemIndex(idx), str_selected.span())
                        }
                        None => {
                            self.selected = TomlValue::new_validation_failed(
                                str_selected.span(),
                                "Invalid item reference".to_string(),
                                None,
                            )
                        }
                    }
                } else {
                    self.selected = TomlValue::new_nested();
                }
            }
            None => {
                self.selected = str_selected.convert_failed_type();
            }
        }
        self
    }
}

#[test]
fn test_adapt_with_alias_primary_name() {
    let toml = "
        items = ['foo', 'bar', 'baz']
        selected = 'bar'
        ";

    let result: Result<_, _> = deserialize::<PartialAliasedAdaptTest, AliasedAdaptTest>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.selected.0, 1);
    }
}

#[test]
fn test_adapt_with_alias_first_alias() {
    let toml = "
        items = ['foo', 'bar', 'baz']
        item_ref = 'baz'
        ";

    let result: Result<_, _> = deserialize::<PartialAliasedAdaptTest, AliasedAdaptTest>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.selected.0, 2);
    }
}

#[test]
fn test_adapt_with_alias_second_alias() {
    let toml = "
        items = ['foo', 'bar', 'baz']
        ref = 'foo'
        ";

    let result: Result<_, _> = deserialize::<PartialAliasedAdaptTest, AliasedAdaptTest>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.selected.0, 0);
    }
}

// Test: verify doesn't call the getter for adapt_in_verify field - fails with "unknown key"
#[tpd]
#[derive(Debug)]
struct ForgotToCallGetter {
    name: String,
    #[tpd_adapt_in_verify]
    converted: ConvertedType,
}

#[derive(Debug)]
struct ConvertedType(i32);

impl VerifyFromToml for PartialForgotToCallGetter {
    fn verify(self, _helper: &mut TomlHelper<'_>) -> Self {
        // Oops! We forgot to call self.tpd_get_converted() at all!
        // This means the key won't be registered as expected
        self
    }
}

#[test]
fn test_adapt_in_verify_forgot_getter_unknown_key() {
    let toml = "
        name = 'test'
        converted = '42'
        ";

    // When verify() doesn't call the getter, the key shows as "Unknown key"
    let result: Result<ForgotToCallGetter, _> =
        deserialize::<PartialForgotToCallGetter, ForgotToCallGetter>(toml);
    dbg!(&result);
    assert!(result.is_err());
    if let Err(e) = result {
        let err_str = format!("{:?}", e);
        assert!(err_str.contains("Unknown key"));
    }
}

// Test: verify calls getter but doesn't set the field - should panic
#[tpd]
#[derive(Debug)]
struct CalledGetterButForgotToSet {
    name: String,
    #[tpd_adapt_in_verify]
    converted: ConvertedType2,
}

#[derive(Debug)]
struct ConvertedType2(i32);

impl VerifyFromToml for PartialCalledGetterButForgotToSet {
    fn verify(self, helper: &mut TomlHelper<'_>) -> Self {
        // We call the getter (which registers the field as expected)
        let _value: TomlValue<String> = self.tpd_get_converted(helper, true);
        // But we forget to set self.converted!
        self
    }
}

#[test]
#[should_panic(expected = "The Partial was still incomplete")]
fn test_adapt_in_verify_not_set_panics() {
    let toml = "
        name = 'test'
        converted = '42'
        ";

    // This should panic because verify() called the getter but didn't set the field
    let _result: Result<CalledGetterButForgotToSet, _> =
        deserialize::<PartialCalledGetterButForgotToSet, CalledGetterButForgotToSet>(toml);
}

// Test: optional adapt_in_verify field (missing_is_error = false)
#[tpd]
#[derive(Debug)]
struct OptionalAdaptTest {
    name: String,
    #[tpd_adapt_in_verify]
    maybe_index: Option<usize>,
}

impl VerifyFromToml for PartialOptionalAdaptTest {
    fn verify(mut self, helper: &mut TomlHelper<'_>) -> Self {
        let str_index: TomlValue<String> = self.tpd_get_maybe_index(helper, false);
        match str_index.as_ref() {
            Some(s) => match s.parse::<usize>() {
                Ok(idx) => self.maybe_index = TomlValue::new_ok(Some(idx), str_index.span()),
                Err(_) => {
                    self.maybe_index = TomlValue::new_validation_failed(
                        str_index.span(),
                        "Not a valid number".to_string(),
                        None,
                    )
                }
            },
            None => {
                // Missing is OK - set to None
                self.maybe_index = TomlValue::new_ok(None, str_index.span())
            }
        }
        self
    }
}

#[test]
fn test_adapt_optional_present() {
    let toml = "
        name = 'test'
        maybe_index = '42'
        ";

    let result: Result<_, _> = deserialize::<PartialOptionalAdaptTest, OptionalAdaptTest>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.maybe_index, Some(42));
    }
}

#[test]
fn test_adapt_optional_missing() {
    let toml = "
        name = 'test'
        ";

    let result: Result<_, _> = deserialize::<PartialOptionalAdaptTest, OptionalAdaptTest>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.maybe_index, None);
    }
}
