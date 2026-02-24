// Regression test: VerifyIn::verify() on a tagged enum variant with Box<Inner> inner type
//
// Bug: `impl<R, T: Visitor + VerifyVisitor<R>> VerifyIn<R> for Box<T>` (visitors.rs)
// uses the default verify() (returns Ok(())) and doesn't forward to T::verify().
//
// Consequence: if a user implements VerifyIn<Parent> for PartialBoxedInner, the verify
// method is silently skipped when the variant is Box<BoxedInner> rather than BoxedInner.

use toml_pretty_deser::prelude::*;
use toml_pretty_deser_macros::tpd;

// --- type definitions ---

#[tpd(tag = "type")]
#[derive(Debug, Eq, PartialEq)]
pub enum BoxTaggedEnum {
    Plain(PlainInner),
    Boxed(Box<BoxedInner>),
}

/// Inner without Box — verify IS called (baseline / non-boxed comparison).
#[tpd]
#[derive(Debug, Eq, PartialEq)]
pub struct PlainInner {
    value: u8,
}

/// Inner wrapped in Box inside the enum variant.
#[tpd]
#[derive(Debug, Eq, PartialEq)]
pub struct BoxedInner {
    value: u8,
}

#[tpd(root, no_verify)]
#[derive(Debug)]
pub struct BoxTaggedRoot {
    #[tpd(nested)]
    item: BoxTaggedEnum,
}

// VerifyIn impls: value == 0 is invalid for both inner types.

impl VerifyIn<PartialBoxTaggedRoot> for PartialPlainInner {
    fn verify(&mut self, _parent: &PartialBoxTaggedRoot) -> Result<(), ValidationFailure> {
        if self.value.value == Some(0) {
            return Err(ValidationFailure::new("PlainInner: value must not be zero", None));
        }
        Ok(())
    }
}

impl VerifyIn<PartialBoxTaggedRoot> for PartialBoxedInner {
    fn verify(&mut self, _parent: &PartialBoxTaggedRoot) -> Result<(), ValidationFailure> {
        if self.value.value == Some(0) {
            return Err(ValidationFailure::new("BoxedInner: value must not be zero", None));
        }
        Ok(())
    }
}

// --- tests ---

/// Baseline: a Plain (non-Box) variant. VerifyIn::verify() should be called and reject value=0.
#[test]
fn test_plain_variant_verify_in_is_called() {
    let toml = "
        [item]
            type = 'Plain'
            value = 0
    ";
    let result = BoxTaggedRoot::tpd_from_toml(
        toml,
        toml_pretty_deser::FieldMatchMode::Exact,
        toml_pretty_deser::VecMode::Strict,
    );
    assert!(
        result.is_err(),
        "expected validation failure for Plain variant with value=0, but got Ok"
    );
}

/// Bug demonstration: the Box variant. VerifyIn::verify() should also be called and reject
/// value=0, but due to the missing forwarding in Box's VerifyIn impl it is silently skipped
/// and the result is Ok instead of an error.
#[test]
fn test_box_variant_verify_in_is_called() {
    let toml = "
        [item]
            type = 'Boxed'
            value = 0
    ";
    let result = BoxTaggedRoot::tpd_from_toml(
        toml,
        toml_pretty_deser::FieldMatchMode::Exact,
        toml_pretty_deser::VecMode::Strict,
    );
    assert!(
        result.is_err(),
        "expected validation failure for Boxed variant with value=0, but VerifyIn::verify() \
         was not forwarded through Box — got Ok instead"
    );
}

/// Sanity check: valid value should succeed for both variants.
#[test]
fn test_box_variant_valid_value_is_ok() {
    let toml = "
        [item]
            type = 'Boxed'
            value = 42
    ";
    let result = BoxTaggedRoot::tpd_from_toml(
        toml,
        toml_pretty_deser::FieldMatchMode::Exact,
        toml_pretty_deser::VecMode::Strict,
    );
    assert!(result.is_ok());
    if let BoxTaggedEnum::Boxed(inner) = result.unwrap().item {
        assert_eq!(inner.value, 42);
    } else {
        panic!("expected Boxed variant");
    }
}
