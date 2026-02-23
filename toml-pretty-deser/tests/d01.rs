//! Test that tagged enums work when inner variant types have specific
//! (non-blanket) `VerifyIn` impls — i.e. the inner types implement
//! `VerifyIn<PartialRoot>` rather than `VerifyIn<R>` for all R.
//!
//! This exercises the `where` clauses on the generated `VerifyVisitor` impl
//! for tagged enums.

use toml_pretty_deser::prelude::*;

// --- Inner types: NOT no_verify, they have specific VerifyIn impls ---

#[tpd]
#[derive(Debug, PartialEq, Eq)]
pub struct Alpha {
    x: u8,
}

#[tpd]
#[derive(Debug, PartialEq, Eq)]
pub struct Beta {
    y: String,
}

// --- Tagged enum using those inner types ---

#[tpd(tag = "kind")]
#[derive(Debug, PartialEq, Eq)]
pub enum MyTagged {
    Alpha(Alpha),
    Beta(Beta),
}

// --- Root struct that contains the tagged enum ---

#[tpd(root)]
#[derive(Debug)]
pub struct Root {
    #[tpd(tagged)]
    item: MyTagged,
}

// VerifyIn impls: inner partials verify against the *root* partial, not a blanket
impl VerifyIn<PartialRoot> for PartialAlpha {}
impl VerifyIn<PartialRoot> for PartialBeta {}

// The tagged enum partial also needs VerifyIn<TPDRoot> (since Root is the tpd root)
impl VerifyIn<TPDRoot> for PartialRoot {}

#[test]
fn tagged_enum_with_specific_verify_in() {
    let toml = r#"
        [item]
        kind = "Alpha"
        x = 42
    "#;
    let parsed =
        Root::tpd_from_toml(toml, FieldMatchMode::Exact, VecMode::Strict).expect("should parse");
    assert_eq!(parsed.item, MyTagged::Alpha(Alpha { x: 42 }));
}

#[test]
fn tagged_enum_with_specific_verify_in_beta() {
    let toml = r#"
        [item]
        kind = "Beta"
        y = "hello"
    "#;
    let parsed =
        Root::tpd_from_toml(toml, FieldMatchMode::Exact, VecMode::Strict).expect("should parse");
    assert_eq!(
        parsed.item,
        MyTagged::Beta(Beta {
            y: "hello".to_string()
        })
    );
}
