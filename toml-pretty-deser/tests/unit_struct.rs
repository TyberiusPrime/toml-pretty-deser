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
        let str_inner: TomlValue<String> = self.tpd_get_inner(helper, true, true);
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
                            self.inner.register_error(&helper.col);
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
        let str_selected: TomlValue<String> = self.tpd_get_selected(helper, true, true);
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
        let _value: TomlValue<String> = self.tpd_get_converted(helper, true, true);
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
        let str_index: TomlValue<String> = self.tpd_get_maybe_index(helper, false, true);
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

// Helper function to extract u8 from either a string (single byte char) or u8 value
pub fn tpd_extract_u8_from_byte_or_char(
    toml_value_str: TomlValue<String>,
    toml_value_u8: TomlValue<u8>,
    missing_is_error: bool,
    helper: &TomlHelper<'_>,
) -> TomlValue<u8> {
    fn err(helper: &TomlHelper<'_>, span: std::ops::Range<usize>) -> TomlValue<u8> {
        let e = TomlValue::new_validation_failed(
            span,
            "Invalid value".to_string(),
            Some("Use a single byte (number or char)".to_string()),
        );
        e.register_error(&helper.col);
        e
    }

    if toml_value_str.is_missing() && toml_value_u8.is_missing() {
        // Register error if the extraction failed (but not for missing - that's ok for optional)
        if !toml_value_u8.is_ok() && (!toml_value_u8.is_missing() || missing_is_error) {
            toml_value_u8.register_error(&helper.col);
        }
        return toml_value_u8;
    }

    match toml_value_str.as_ref() {
        Some(s) => {
            if s.as_bytes().len() != 1 {
                err(helper, toml_value_str.span())
            } else {
                TomlValue::new_ok(s.as_bytes()[0], toml_value_str.span())
            }
        }
        None => match toml_value_u8.as_ref() {
            Some(byte) => TomlValue::new_ok(*byte, toml_value_u8.span()),
            None => err(helper, toml_value_u8.span()),
        },
    }
}

// Test: adapt_in_verify with Option<u8> field using byte-or-char extraction
#[tpd]
#[derive(Debug)]
struct ByteOrCharTest {
    name: String,
    #[tpd_adapt_in_verify]
    delimiter: Option<u8>,
}

impl VerifyFromToml for PartialByteOrCharTest {
    fn verify(mut self, helper: &mut TomlHelper<'_>) -> Self {
        // Use tpd_get_delimiter with auto_register_type_errors=false to avoid registering
        // WrongType errors when probing for multiple types
        let extracted = tpd_extract_u8_from_byte_or_char(
            self.tpd_get_delimiter(helper, false, false),
            self.tpd_get_delimiter(helper, false, false),
            false,
            helper,
        );
        self.delimiter = extracted.into_optional();
        self
    }
}

#[test]
fn test_byte_or_char_with_char() {
    let toml = r#"
        name = "test"
        delimiter = ","
    "#;

    let result: Result<_, _> = deserialize::<PartialByteOrCharTest, ByteOrCharTest>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.delimiter, Some(b','));
    }
}

#[test]
fn test_byte_or_char_with_number() {
    let toml = r#"
        name = "test"
        delimiter = 44
    "#;

    let result: Result<_, _> = deserialize::<PartialByteOrCharTest, ByteOrCharTest>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.delimiter, Some(44)); // ASCII for ','
    }
}

#[test]
fn test_byte_or_char_missing() {
    let toml = r#"
        name = "test"
    "#;

    let result: Result<_, _> = deserialize::<PartialByteOrCharTest, ByteOrCharTest>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.delimiter, None);
    }
}

#[test]
fn test_byte_or_char_invalid_multi_char_string() {
    let toml = r#"
        name = "test"
        delimiter = "abc"
    "#;

    let result: Result<_, _> = deserialize::<PartialByteOrCharTest, ByteOrCharTest>(toml);
    dbg!(&result);
    assert!(result.is_err());
    if let Err(e) = result {
        let err_str = format!("{:?}", e);
        assert!(err_str.contains("Invalid value"));
    }
}

// Test: adapt_in_verify with tpd_default - the adapt getter should use missing_is_error=false
#[tpd]
#[derive(Debug)]
struct AdaptWithDefaultTest {
    name: String,
    #[tpd_adapt_in_verify]
    #[tpd_default]
    converted: ConvertedTypeDefault,
}

#[derive(Debug, Default)]
struct ConvertedTypeDefault(i32);

impl VerifyFromToml for PartialAdaptWithDefaultTest {
    fn verify(mut self, helper: &mut TomlHelper<'_>) -> Self {
        // This should NOT produce a "Missing required key" error when the field is missing
        // because tpd_default means missing is ok
        let str_val: TomlValue<String> = self.tpd_get_converted(helper, false, true);
        match str_val.as_ref() {
            Some(s) => match s.parse::<i32>() {
                Ok(n) => {
                    self.converted = TomlValue::new_ok(ConvertedTypeDefault(n), str_val.span())
                }
                Err(_) => {
                    self.converted = TomlValue::new_validation_failed(
                        str_val.span(),
                        "Not a valid number".to_string(),
                        None,
                    )
                }
            },
            None => {
                // Missing - use default
                self.converted = TomlValue::new_ok(ConvertedTypeDefault::default(), str_val.span())
            }
        }
        self
    }
}

#[test]
fn test_adapt_with_default_present() {
    let toml = "
        name = 'test'
        converted = '42'
        ";

    let result: Result<_, _> =
        deserialize::<PartialAdaptWithDefaultTest, AdaptWithDefaultTest>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.converted.0, 42);
    }
}

#[test]
fn test_adapt_with_default_missing() {
    let toml = "
        name = 'test'
        ";

    let result: Result<_, _> =
        deserialize::<PartialAdaptWithDefaultTest, AdaptWithDefaultTest>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.converted.0, 0); // default
    }
}

// Test: adapt_in_verify with tpd_default_in_verify - combined behavior
#[tpd]
#[derive(Debug)]
struct AdaptWithDefaultInVerifyTest {
    name: String,
    #[tpd_adapt_in_verify]
    #[tpd_default_in_verify]
    converted: ConvertedTypeDefault2,
}

#[derive(Debug, Default)]
struct ConvertedTypeDefault2(i32);

impl VerifyFromToml for PartialAdaptWithDefaultInVerifyTest {
    fn verify(mut self, helper: &mut TomlHelper<'_>) -> Self {
        // With both adapt_in_verify AND default_in_verify, the getter should ideally
        // not generate a "Missing required key" error automatically
        let str_val: TomlValue<String> = self.tpd_get_converted(helper, false, true);
        match str_val.as_ref() {
            Some(s) => match s.parse::<i32>() {
                Ok(n) => {
                    self.converted = TomlValue::new_ok(ConvertedTypeDefault2(n), str_val.span())
                }
                Err(_) => {
                    self.converted = TomlValue::new_validation_failed(
                        str_val.span(),
                        "Not a valid number".to_string(),
                        None,
                    )
                }
            },
            None => {
                // Missing - use default
                self.converted = TomlValue::new_ok(ConvertedTypeDefault2::default(), str_val.span())
            }
        }
        self
    }
}

#[test]
fn test_adapt_with_default_in_verify_missing() {
    let toml = "
        name = 'test'
        ";

    let result: Result<_, _> =
        deserialize::<PartialAdaptWithDefaultInVerifyTest, AdaptWithDefaultInVerifyTest>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.converted.0, 0); // default
    }
}

// Test: adapt_in_verify with tpd_default - missing_is_error parameter is ignored
// When tpd_default is present alongside tpd_adapt_in_verify, the getter automatically
// forces missing_is_error to false, regardless of what the caller passes.
#[tpd]
#[derive(Debug)]
struct AdaptWithDefaultIgnoresErrorFlagTest {
    name: String,
    #[tpd_adapt_in_verify]
    #[tpd_default]
    converted: ConvertedTypeDefault3,
}

#[derive(Debug, Default)]
struct ConvertedTypeDefault3(i32);

impl VerifyFromToml for PartialAdaptWithDefaultIgnoresErrorFlagTest {
    fn verify(mut self, helper: &mut TomlHelper<'_>) -> Self {
        // Even though we pass missing_is_error=true, the tpd_default attribute
        // ensures that no "Missing required key" error is generated
        let str_val: TomlValue<String> = self.tpd_get_converted(helper, true, true);
        match str_val.as_ref() {
            Some(s) => match s.parse::<i32>() {
                Ok(n) => {
                    self.converted = TomlValue::new_ok(ConvertedTypeDefault3(n), str_val.span())
                }
                Err(_) => {
                    self.converted = TomlValue::new_validation_failed(
                        str_val.span(),
                        "Not a valid number".to_string(),
                        None,
                    )
                }
            },
            None => {
                // Missing - use default
                self.converted = TomlValue::new_ok(ConvertedTypeDefault3::default(), str_val.span())
            }
        }
        self
    }
}

#[test]
fn test_adapt_with_default_ignores_error_flag_present() {
    // When the field is present, it should parse correctly
    let toml = "
        name = 'test'
        converted = '42'
        ";

    let result: Result<_, _> = deserialize::<
        PartialAdaptWithDefaultIgnoresErrorFlagTest,
        AdaptWithDefaultIgnoresErrorFlagTest,
    >(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.converted.0, 42);
    }
}

#[test]
fn test_adapt_with_default_ignores_error_flag_missing() {
    // Even with missing_is_error=true in the getter call, tpd_default ensures
    // no "Missing required key" error is generated
    let toml = "
        name = 'test'
        ";

    let result: Result<_, _> = deserialize::<
        PartialAdaptWithDefaultIgnoresErrorFlagTest,
        AdaptWithDefaultIgnoresErrorFlagTest,
    >(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.converted.0, 0); // default
    }
}

// Test: adapt_in_verify with tpd_default_in_verify - missing_is_error parameter is also ignored
#[tpd]
#[derive(Debug)]
struct AdaptWithDefaultInVerifyIgnoresErrorFlagTest {
    name: String,
    #[tpd_adapt_in_verify]
    #[tpd_default_in_verify]
    converted: ConvertedTypeDefault4,
}

#[derive(Debug, Default)]
struct ConvertedTypeDefault4(i32);

impl VerifyFromToml for PartialAdaptWithDefaultInVerifyIgnoresErrorFlagTest {
    fn verify(mut self, helper: &mut TomlHelper<'_>) -> Self {
        // Even though we pass missing_is_error=true, the tpd_default_in_verify attribute
        // ensures that no "Missing required key" error is generated
        let str_val: TomlValue<String> = self.tpd_get_converted(helper, true, true);
        match str_val.as_ref() {
            Some(s) => match s.parse::<i32>() {
                Ok(n) => {
                    self.converted = TomlValue::new_ok(ConvertedTypeDefault4(n), str_val.span())
                }
                Err(_) => {
                    self.converted = TomlValue::new_validation_failed(
                        str_val.span(),
                        "Not a valid number".to_string(),
                        None,
                    )
                }
            },
            None => {
                // Missing - use default
                self.converted = TomlValue::new_ok(ConvertedTypeDefault4::default(), str_val.span())
            }
        }
        self
    }
}

#[test]
fn test_adapt_with_default_in_verify_ignores_error_flag_missing() {
    // Even with missing_is_error=true in the getter call, tpd_default_in_verify ensures
    // no "Missing required key" error is generated
    let toml = "
        name = 'test'
        ";

    let result: Result<_, _> = deserialize::<
        PartialAdaptWithDefaultInVerifyIgnoresErrorFlagTest,
        AdaptWithDefaultInVerifyIgnoresErrorFlagTest,
    >(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.converted.0, 0); // default
    }
}

// Test: adapt_in_verify WITHOUT tpd_default - missing_is_error parameter is respected
// This ensures the fix doesn't break the normal behavior when no default is specified
#[tpd]
#[derive(Debug)]
struct AdaptWithoutDefaultTest {
    name: String,
    #[tpd_adapt_in_verify]
    converted: ConvertedTypeNoDefault,
}

#[derive(Debug)]
struct ConvertedTypeNoDefault(i32);

impl VerifyFromToml for PartialAdaptWithoutDefaultTest {
    fn verify(mut self, helper: &mut TomlHelper<'_>) -> Self {
        // Without tpd_default, missing_is_error=true should produce an error when missing
        let str_val: TomlValue<String> = self.tpd_get_converted(helper, true, true);
        match str_val.as_ref() {
            Some(s) => match s.parse::<i32>() {
                Ok(n) => {
                    self.converted = TomlValue::new_ok(ConvertedTypeNoDefault(n), str_val.span())
                }
                Err(_) => {
                    self.converted = TomlValue::new_validation_failed(
                        str_val.span(),
                        "Not a valid number".to_string(),
                        None,
                    )
                }
            },
            None => {
                // For required field, we still set a failed state to trigger error
                self.converted = str_val.convert_failed_type();
            }
        }
        self
    }
}

#[test]
fn test_adapt_without_default_present() {
    let toml = "
        name = 'test'
        converted = '42'
        ";

    let result: Result<_, _> =
        deserialize::<PartialAdaptWithoutDefaultTest, AdaptWithoutDefaultTest>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.converted.0, 42);
    }
}

#[test]
fn test_adapt_without_default_missing() {
    // Without tpd_default, a missing field should produce an error
    let toml = "
        name = 'test'
        ";

    let result: Result<_, _> =
        deserialize::<PartialAdaptWithoutDefaultTest, AdaptWithoutDefaultTest>(toml);
    dbg!(&result);
    assert!(result.is_err());
    if let Err(e) = &result {
        let err_str = format!("{:?}", e);
        assert!(err_str.contains("Missing required key"));
    }
}
