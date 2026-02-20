use std::{cell::RefCell, rc::Rc};

use toml_pretty_deser::prelude::*;

/// Tests that are not straight ports from 'manual impl' a01 to 'macro impl' b01
///
#[tpd(root)]
#[derive(Debug)]
#[allow(dead_code)]
struct Outer {
    value: u8,
}

impl VerifyIn<TPDRoot> for PartialOuter {}

#[test]
fn test_deser_error_std_error_impl() {
    use std::error::Error;

    // ParsingFailure: Display mentions "TOML parse error", source() is Some
    let result: Result<Outer, _> = Outer::tpd_from_toml(
        "this is not valid toml ===",
        FieldMatchMode::Exact,
        VecMode::Strict,
    );
    assert!(result.is_err());
    if let Err(e) = result {
        assert!(
            e.to_string().contains("TOML parse error"),
            "unexpected Display: {e}"
        );
        assert!(e.source().is_some(), "ParsingFailure should have a source");
    }

    // DeserFailure: Display mentions "deserialization failed", source() is None
    let result: Result<Outer, _> = Outer::tpd_from_toml(
        "# all required fields missing",
        FieldMatchMode::Exact,
        VecMode::Strict,
    );
    assert!(result.is_err());
    if let Err(e) = result {
        assert!(
            e.to_string().contains("deserialization failed"),
            "unexpected Display: {e}"
        );
        assert!(e.source().is_none(), "DeserFailure should have no source");
    }
}

#[tpd(root)]
#[derive(Debug)]
#[allow(dead_code)]
struct OuterWithRc {
    #[tpd(adapt_in_verify(u8))]
    value: Rc<RefCell<u8>>,

    #[tpd(nested, adapt_in_verify)]
    outer: std::sync::Arc<Outer>,

    #[tpd(nested, adapt_in_verify)]
    outer_rc: Rc<RefCell<Outer>>,
}

impl VerifyIn<PartialOuterWithRc> for PartialOuter {}

impl VerifyIn<TPDRoot> for PartialOuterWithRc {
    fn verify(&mut self, _parent: &TPDRoot) -> Result<(), ValidationFailure>
    where
        Self: Sized + toml_pretty_deser::Visitor,
    {
        self.value
            .adapt(|value, span| TomlValue::new_ok(Rc::new(RefCell::new(value)), span));
        self.outer
            .adapt(|outer, span| TomlValue::new_ok(std::sync::Arc::new(outer), span));
        self.outer_rc
            .adapt(|outer, span| TomlValue::new_ok(Rc::new(RefCell::new(outer)), span));
        Ok(())
    }
}

#[test]
fn test_refcell() {
    let result = OuterWithRc::tpd_from_toml(
        "value = 23
        outer.value = 33
        outer_rc.value = 44
",
        FieldMatchMode::Exact,
        VecMode::Strict,
    );
    if let Ok(parsed) = result {
        assert_eq!(*parsed.value.borrow(), 23);
        assert_eq!(parsed.outer.value, 33);
        assert_eq!(parsed.outer_rc.borrow().value, 44);
    } else {
        panic!("Unexpected error: {result:?}");
    }
}

#[tpd(root, no_verify)]
#[derive(Debug)]
pub struct WithSkippedHashmap {
    name: String,
    #[tpd(skip)]
    ignored: std::collections::HashMap<String, String>,
}

#[test]
fn test_skip_hashmap_field() {
    let toml_str = "
        name = 'hello'
    ";
    let parsed =
        WithSkippedHashmap::tpd_from_toml(toml_str, FieldMatchMode::Exact, VecMode::Strict);
    assert!(parsed.is_ok());
    if let Ok(parsed) = parsed {
        assert_eq!(parsed.name, "hello");
        assert!(parsed.ignored.is_empty()); // Default::default() for HashMap
    }
}

pub fn adapt_to_upper_case(input: TomlValue<String>) -> TomlValue<String> {
    input.map(|s| s.to_uppercase())
}

// Verify that `with` on an Option<T> field passes T (not Option<T>) to the adapter function.
// `adapt_to_upper_case` takes TomlValue<String> -> TomlValue<String>; if it saw Option<String>
// the field would fail to compile.
#[tpd(root, no_verify)]
#[derive(Debug)]
struct WithOnOption {
    #[tpd(with = "adapt_to_upper_case")]
    required: String,
    #[tpd(with = "adapt_to_upper_case")]
    optional: Option<String>,
}

#[test]
fn test_with_on_option_sees_inner_t() {
    // Both present: adapter is applied to the inner string
    let toml = "required = 'hello'\noptional = 'world'";
    let parsed = WithOnOption::tpd_from_toml(toml, FieldMatchMode::Exact, VecMode::Strict)
        .expect("should parse");
    assert_eq!(parsed.required, "HELLO");
    assert_eq!(parsed.optional, Some("WORLD".to_string()));

    // optional absent: field becomes None, not an error
    let toml = "required = 'hello'";
    let parsed = WithOnOption::tpd_from_toml(toml, FieldMatchMode::Exact, VecMode::Strict)
        .expect("should parse with optional absent");
    assert_eq!(parsed.required, "HELLO");
    assert_eq!(parsed.optional, None);
}
