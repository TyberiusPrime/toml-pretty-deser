// Regression test: AnyCase matching resolves a TOML tag value "Sample"
// (UpperCamelCase) against a tagged-enum alias defined as `#[tpd(alias = "sample")]`
// (all-lowercase).  Under AnyCase, both normalize to "sample", so the match
// must succeed.

use toml_pretty_deser::prelude::*;
use toml_pretty_deser_macros::tpd;

// ─── types ────────────────────────────────────────────────────────────────────

#[tpd(no_verify)]
#[derive(Debug, PartialEq, Eq)]
pub struct AlphaInner {
    value: u32,
}

#[tpd(no_verify)]
#[derive(Debug, PartialEq, Eq)]
pub struct BetaInner {
    count: u32,
}

/// Tagged enum where the `Alpha` variant carries a lowercase alias `"sample"`.
/// The TOML will supply `"Sample"` (UpperCamelCase); AnyCase must match them.
#[tpd(tag = "kind")]
#[derive(Debug, PartialEq, Eq)]
pub enum ShapeEnum {
    #[tpd(alias = "sample")]
    Alpha(AlphaInner),
    Beta(BetaInner),
}

#[tpd(root, no_verify)]
#[derive(Debug)]
pub struct Root {
    #[tpd(nested)]
    shape: ShapeEnum,
}

// ─── tests ────────────────────────────────────────────────────────────────────

/// "Sample" in TOML matches alias `"sample"` on the `Alpha` variant via AnyCase.
#[test]
fn test_anycase_alias_sample_matches_alpha() {
    let toml = r#"
        [shape]
        kind = "Sample"
        value = 42
    "#;

    let result = Root::tpd_from_toml(toml, FieldMatchMode::AnyCase, VecMode::Strict);
    assert!(
        result.is_ok(),
        "\"Sample\" should match alias \"sample\" under AnyCase, got: {:?}",
        result
    );
    if let Ok(root) = result {
        assert_eq!(root.shape, ShapeEnum::Alpha(AlphaInner { value: 42 }));
    }
}

/// Exact mode must NOT match "Sample" against alias "sample" (different case).
#[test]
fn test_exact_alias_sample_does_not_match() {
    let toml = r#"
        [shape]
        kind = "Sample"
        value = 42
    "#;

    let result = Root::tpd_from_toml(toml, FieldMatchMode::Exact, VecMode::Strict);
    assert!(
        result.is_err(),
        "\"Sample\" must not match alias \"sample\" under Exact mode"
    );
}

/// The lowercase literal "sample" itself must also match under AnyCase.
#[test]
fn test_anycase_lowercase_sample_matches_alpha() {
    let toml = r#"
        [shape]
        kind = "sample"
        value = 7
    "#;

    let result = Root::tpd_from_toml(toml, FieldMatchMode::AnyCase, VecMode::Strict);
    assert!(
        result.is_ok(),
        "lowercase \"sample\" should match alias \"sample\" under AnyCase, got: {:?}",
        result
    );
    if let Ok(root) = result {
        assert_eq!(root.shape, ShapeEnum::Alpha(AlphaInner { value: 7 }));
    }
}

/// The lowercase literal "sample" must match under Exact mode (alias matches exactly).
#[test]
fn test_exact_lowercase_sample_matches_alpha() {
    let toml = r#"
        [shape]
        kind = "sample"
        value = 99
    "#;

    let result = Root::tpd_from_toml(toml, FieldMatchMode::Exact, VecMode::Strict);
    assert!(
        result.is_ok(),
        "lowercase \"sample\" should match alias \"sample\" under Exact mode, got: {:?}",
        result
    );
    if let Ok(root) = result {
        assert_eq!(root.shape, ShapeEnum::Alpha(AlphaInner { value: 99 }));
    }
}
