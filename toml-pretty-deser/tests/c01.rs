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

#[tpd(root, no_verify)]
#[derive(Debug)]
struct TakeFloat {
    value: f64,
}

#[test]
fn test_float_takes_0_int() {
    let result: Result<TakeFloat, _> =
        TakeFloat::tpd_from_toml("value = 0", FieldMatchMode::Exact, VecMode::Strict);
    assert!(result.is_ok(), "should int 0 as float 0.0");
    if let Ok(parsed) = result {
        assert_eq!(parsed.value, 0.0);
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
            .adapt(|value| (Rc::new(RefCell::new(value)), TomlValueState::Ok));
        self.outer
            .adapt(|outer| (std::sync::Arc::new(outer), TomlValueState::Ok));
        self.outer_rc
            .adapt(|outer| (Rc::new(RefCell::new(outer)), TomlValueState::Ok));
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
    #[tpd(skip, default)]
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

#[derive(Debug)]
struct NotAString(String);

fn adapt_not_a_string(input: TomlValue<String>) -> TomlValue<NotAString> {
    input.map(NotAString)
}
fn adapt_not_a_string_from_toml_item(input: TomlValue<toml_edit::Item>) -> TomlValue<NotAString> {
    input.map(|value| NotAString(value.to_string().trim().to_string()))
}

#[tpd(root, no_verify)]
struct AdaptTest {
    #[tpd(with = "adapt_not_a_string")]
    a: NotAString,
    #[tpd(with = "adapt_not_a_string_from_toml_item")]
    b: NotAString,
}

#[test]
fn test_with_non_string() {
    let toml = "a = 'hello'
    b = 23";
    let parsed = AdaptTest::tpd_from_toml(toml, FieldMatchMode::Exact, VecMode::Strict)
        .expect("should parse");
    assert_eq!(parsed.a.0, "hello");
    assert_eq!(parsed.b.0, "23");
}

// Verify that `with` on a Vec<T> field calls the adapter on each element (not the whole Vec).
#[tpd(root, no_verify)]
#[derive(Debug)]
struct WithOnVec {
    #[tpd(with = "adapt_to_upper_case")]
    items: Vec<String>,
}

#[test]
fn test_with_on_vec_applies_per_element() {
    let toml = "items = ['hello', 'world']";
    let parsed = WithOnVec::tpd_from_toml(toml, FieldMatchMode::Exact, VecMode::Strict)
        .expect("should parse");
    assert_eq!(parsed.items, vec!["HELLO".to_string(), "WORLD".to_string()]);

    // empty vec: adapter is never called, result is empty
    let toml = "items = []";
    let parsed = WithOnVec::tpd_from_toml(toml, FieldMatchMode::Exact, VecMode::Strict)
        .expect("should parse with empty vec");
    assert!(parsed.items.is_empty());
}

// Verify that `with` on an IndexMap<K, V> field calls the adapter on each value (not the whole map).
#[tpd(root, no_verify)]
#[derive(Debug)]
struct WithOnMap {
    #[tpd(with = "adapt_to_upper_case")]
    items: indexmap::IndexMap<String, String>,
}

#[test]
fn test_with_on_map_applies_per_value() {
    let toml = "[items]\nhello = 'world'\nfoo = 'bar'";
    let parsed = WithOnMap::tpd_from_toml(toml, FieldMatchMode::Exact, VecMode::Strict)
        .expect("should parse");
    assert_eq!(parsed.items["hello"], "WORLD");
    assert_eq!(parsed.items["foo"], "BAR");
}

// Verify that `with` on Vec<T> works when the TARGET type is not Visitor (only the source needs to be).
#[tpd(root, no_verify)]
#[derive(Debug)]
struct WithOnVecNonVisitor {
    #[tpd(with = "adapt_not_a_string")]
    items: Vec<NotAString>,
}

#[test]
fn test_with_on_vec_non_visitor_target() {
    let toml = "items = ['hello', 'world']";
    let parsed = WithOnVecNonVisitor::tpd_from_toml(toml, FieldMatchMode::Exact, VecMode::Strict)
        .expect("should parse");
    assert_eq!(parsed.items[0].0, "hello");
    assert_eq!(parsed.items[1].0, "world");
}

// Verify that `with` on IndexMap<K, V> works when the TARGET value type is not Visitor.
#[tpd(root, no_verify)]
#[derive(Debug)]
struct WithOnMapNonVisitor {
    #[tpd(with = "adapt_not_a_string")]
    items: indexmap::IndexMap<String, NotAString>,
}

#[test]
fn test_with_on_map_non_visitor_target() {
    let toml = "[items]\nhello = 'world'\nfoo = 'bar'";
    let parsed = WithOnMapNonVisitor::tpd_from_toml(toml, FieldMatchMode::Exact, VecMode::Strict)
        .expect("should parse");
    assert_eq!(parsed.items["hello"].0, "world");
    assert_eq!(parsed.items["foo"].0, "bar");
}

/// test that option<nested> calls verify in?
///
///
mod test_option_nested {
    use indexmap::IndexMap;
    use toml_pretty_deser::prelude::*;
    #[tpd]
    struct Inner {
        a: u8,
    }

    #[tpd(root)]
    struct Outer {
        #[tpd(nested)]
        inner: Option<Inner>,
        #[tpd(nested)]
        v_inner: Option<Vec<Inner>>,

        #[tpd(nested)]
        m_inner: Option<IndexMap<String, Inner>>,

        #[tpd(nested)]
        mv_inner: Option<IndexMap<String, Vec<Inner>>>,
    }

    impl VerifyIn<TPDRoot> for PartialOuter {}

    impl VerifyIn<PartialOuter> for PartialInner {
        fn verify(&mut self, _parent: &PartialOuter) -> Result<(), ValidationFailure>
        where
            Self: Sized + toml_pretty_deser::Visitor,
        {
            self.a.verify(|a| {
                if *a == 0 {
                    Err(ValidationFailure::new("Can not be zero", None))
                } else {
                    Ok(())
                }
            });
            Ok(())
        }
    }

    #[test]
    fn verify_called_on_option_nested() {
        // valid case: inner is present and valid
        let toml = "inner.a = 5
            v_inner = [{a=6}]
            m_inner.key = {a=7}
            mv_inner.key = [{a=8}, {a=9}]
        ";
        let result = Outer::tpd_from_toml(toml, FieldMatchMode::Exact, VecMode::Strict);
        assert!(result.is_ok(), "should parse with valid inner");
        let result = result.unwrap();
        assert_eq!(result.inner.unwrap().a, 5);
        assert_eq!(result.v_inner.unwrap()[0].a, 6);
        assert_eq!(result.m_inner.unwrap()["key"].a, 7);
        assert_eq!(result.mv_inner.unwrap()["key"][0].a, 8);

        // valid case: inner is absent (Option::None), should not call verify and should succeed
        let toml = "";
        let result = Outer::tpd_from_toml(toml, FieldMatchMode::Exact, VecMode::Strict);
        assert!(result.is_ok(), "should parse with missing inner");

        // invalid case: inner is present but invalid (a == 0), should call verify and fail
        //   // invalid case: inner is present but invalid (a == 0), should call verify and fail
        let toml = "inner.a = 0";
        let result = Outer::tpd_from_toml(toml, FieldMatchMode::Exact, VecMode::Strict);
        assert!(result.is_err(), "should fail with invalid inner");
        let toml = "v_inner = [{a=0}]";
        let result = Outer::tpd_from_toml(toml, FieldMatchMode::Exact, VecMode::Strict);
        assert!(result.is_err(), "should fail with invalid inner");

        let toml = "v_inner = {a=0}";
        let result = Outer::tpd_from_toml(toml, FieldMatchMode::Exact, VecMode::SingleOk);
        assert!(result.is_err(), "should fail with invalid inner");

        let toml = "m_inner.key = {a=0}";
        let result = Outer::tpd_from_toml(toml, FieldMatchMode::Exact, VecMode::Strict);
        assert!(result.is_err(), "should fail with invalid inner");

        let toml = "mv_inner.key = [{a=0}]";
        let result = Outer::tpd_from_toml(toml, FieldMatchMode::Exact, VecMode::Strict);
        assert!(result.is_err(), "should fail with invalid inner");

        let toml = "v_inner = [{a=5}, {a=3}, {a=0}]";
        let result = Outer::tpd_from_toml(toml, FieldMatchMode::Exact, VecMode::SingleOk);
        assert!(result.is_err(), "should fail with invalid inner");
        if let Err(e) = result {
            insta::assert_snapshot!(e.pretty("test.toml"));
        }
    }
}
