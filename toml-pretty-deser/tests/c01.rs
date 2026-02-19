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

impl VerifyIn<Root> for PartialOuter {}

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

impl VerifyIn<Root> for PartialOuterWithRc {
    fn verify(&mut self, _parent: &Root) -> Result<(), ValidationFailure>
    where
        Self: Sized + toml_pretty_deser::Visitor,
    {
        self.value = self
            .value
            .take()
            .adapt(|value, span| TomlValue::new_ok(Rc::new(RefCell::new(value)), span));
        self.outer = self
            .outer
            .take()
            .adapt(|outer, span| TomlValue::new_ok(std::sync::Arc::new(outer), span));
        self.outer_rc = self
            .outer_rc
            .take()
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
