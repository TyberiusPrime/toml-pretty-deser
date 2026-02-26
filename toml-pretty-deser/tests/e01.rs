#![allow(clippy::must_use_candidate)]

use toml_pretty_deser::prelude::*;
use toml_pretty_deser_macros::tpd;

// ============================================================================
// Help context propagation — struct nesting
// ============================================================================

#[tpd(root, no_verify)]
#[derive(Debug)]
#[allow(dead_code)]
pub struct HelpCtxRoot {
    top_value: u8,
    #[tpd(nested)]
    section: HelpCtxSection,
}

#[tpd(no_verify)]
#[derive(Debug)]
#[allow(dead_code)]
pub struct HelpCtxSection {
    #[tpd(nested)]
    subsection: HelpCtxSubSection,
}

#[tpd(no_verify)]
#[derive(Debug)]
#[allow(dead_code)]
pub struct HelpCtxSubSection {
    inner_value: u8,
}

/// A Nested `TomlValue`'s `help` field should propagate into the error context so that
/// all descendant errors carry the hint text.  Two nesting levels are used to verify
/// that the ordering is innermost-first (the order in which context was pushed, reversed).
///
/// Intended usage pattern:
/// 1. Deserialise (fails because `inner_value` is missing).
/// 2. Post-fail: call `set_help` on each Nested `TomlValue` inside the partial.
/// 3. Render errors — both help strings must appear in the child error's hint,
///    innermost first.
#[test]
fn test_help_context_propagates_to_nested_children() {
    let toml = "
        top_value = 1
        [section.subsection]
            # inner_value is intentionally missing
    ";

    let result = HelpCtxRoot::tpd_from_toml(
        toml,
        toml_pretty_deser::FieldMatchMode::Exact,
        toml_pretty_deser::VecMode::Strict,
    );

    assert!(result.is_err(), "should fail: inner_value is missing");

    if let Err(mut e) = result {
        // Post-fail modification: attach documentation URLs to both nesting levels.
        if let DeserError::DeserFailure(_, ref mut partial) = e {
            if let Some(root_inner) = partial.value.as_mut() {
                root_inner
                    .section
                    .set_help("See: https://docs.example.com/section");
                if let Some(section_inner) = root_inner.section.value.as_mut() {
                    section_inner
                        .subsection
                        .set_help("See: https://docs.example.com/subsection");
                }
            }
        }
        // Both help texts must appear; inner (subsection) comes before outer (section)
        // because the context stack is reversed on output (innermost-first).
        let pretty = e.pretty("test.toml");
        let section_pos = pretty
            .find("docs.example.com/section")
            .expect("outer help not found");
        let subsection_pos = pretty
            .find("docs.example.com/subsection")
            .expect("inner help not found");
        assert!(
            subsection_pos < section_pos,
            "expected inner help before outer help, got:\n{pretty}"
        );
        insta::assert_snapshot!(pretty);
    }
}

#[test]
fn test_help_context_propagates_to_nested_children_other_error() {
    let toml = "
        top_value = 1
        [section.subsection]
            inner_value = 'no'
    ";

    let result = HelpCtxRoot::tpd_from_toml(
        toml,
        toml_pretty_deser::FieldMatchMode::Exact,
        toml_pretty_deser::VecMode::Strict,
    );

    assert!(result.is_err(), "should fail: inner_value is missing");

    if let Err(mut e) = result {
        // Post-fail modification: attach documentation URLs to both nesting levels.
        if let DeserError::DeserFailure(_, ref mut partial) = e {
            if let Some(root_inner) = partial.value.as_mut() {
                root_inner
                    .section
                    .set_help("See: https://docs.example.com/section");
                if let Some(section_inner) = root_inner.section.value.as_mut() {
                    section_inner
                        .subsection
                        .set_help("See: https://docs.example.com/subsection");
                }
            }
        }
        // Both help texts must appear; inner (subsection) comes before outer (section)
        // because the context stack is reversed on output (innermost-first).
        let pretty = e.pretty("test.toml");
        insta::assert_snapshot!(pretty);
        let section_pos = pretty
            .find("docs.example.com/section")
            .expect("outer help not found");
        let subsection_pos = pretty
            .find("docs.example.com/subsection")
            .expect("inner help not found");
        assert!(
            subsection_pos < section_pos,
            "expected inner help before outer help, got:\n{pretty}"
        );
    }
}

#[test]
fn test_help_context_propagates_to_nested_children_other_error2() {
    let toml = "
        top_value = 1
        [section.subsection]
            xnner_value = 23
    ";

    let result = HelpCtxRoot::tpd_from_toml(
        toml,
        toml_pretty_deser::FieldMatchMode::Exact,
        toml_pretty_deser::VecMode::Strict,
    );

    assert!(result.is_err(), "should fail: inner_value is missing");

    if let Err(mut e) = result {
        // Post-fail modification: attach documentation URLs to both nesting levels.
        if let DeserError::DeserFailure(_, ref mut partial) = e {
            if let Some(root_inner) = partial.value.as_mut() {
                root_inner
                    .section
                    .set_help("See: https://docs.example.com/section");
                if let Some(section_inner) = root_inner.section.value.as_mut() {
                    section_inner
                        .subsection
                        .set_help("See: https://docs.example.com/subsection");
                }
            }
        }
        // Both help texts must appear; inner (subsection) comes before outer (section)
        // because the context stack is reversed on output (innermost-first).
        dbg!(&e);
        let pretty = e.pretty("test.toml");
        insta::assert_snapshot!(pretty);
        let section_pos = pretty
            .find("docs.example.com/section")
            .expect("outer help not found");
        let subsection_pos = pretty
            .find("docs.example.com/subsection")
            .expect("inner help not found");
        assert!(
            subsection_pos < section_pos,
            "expected inner help before outer help, got:\n{pretty}"
        );
    }
}

// ============================================================================
// Help context propagation — Custom state inside Option<Vec<T>> (Nested Vec)
// ============================================================================

/// A leaf visitor type whose `fill_from_toml` can produce a `Custom`-state
/// `TomlValue` (multi-span error) when the string value doesn't start with "ok:".
/// This lets us test that `context_help` propagates through a `Nested` Vec
/// all the way to a leaf `Custom` error.
#[derive(Debug)]
#[allow(dead_code)]
struct PrefixedVal(String);

toml_pretty_deser::impl_visitor!(PrefixedVal, true, |helper| {
    let span = helper.span();
    match helper.item.as_str() {
        Some(v) if v.starts_with("ok:") => TomlValue::new_ok(PrefixedVal(v.to_string()), span),
        Some(v) => TomlValue::new_custom(
            None,
            vec![(span, format!("Value '{}' must start with 'ok:'", v))],
            None,
        ),
        None => TomlValue::new_wrong_type(&helper.item, span, "string"),
    }
});

#[tpd(root, no_verify)]
#[derive(Debug)]
#[allow(dead_code)]
pub struct CustomStateInOptVec {
    items: Option<Vec<PrefixedVal>>,
}

/// When a Vec element is in `Custom` state and the outer `Option<Vec<...>>`
/// is in `Nested` state, `context_help` set on the outer field must appear
/// in the rendered error for the inner `Custom` error.
#[test]
fn test_custom_state_in_opt_vec_context_propagated() {
    let toml_str = r#"items = ["ok:first", "bad_value", "ok:third"]"#;

    let result =
        CustomStateInOptVec::tpd_from_toml(toml_str, FieldMatchMode::Exact, VecMode::Strict);
    assert!(
        result.is_err(),
        "should fail: 'bad_value' lacks required 'ok:' prefix"
    );

    if let Err(mut e) = result {
        if let DeserError::DeserFailure(_, ref mut partial) = e {
            if let Some(root) = partial.value.as_mut() {
                // items is Nested (one element has Custom state); set help on it.
                root.items.set_help("See: https://docs.example.com/items");
            }
        }

        let pretty = e.pretty("test.toml");
        insta::assert_snapshot!(pretty);
        assert!(
            pretty.contains("docs.example.com/items"),
            "context help must propagate through Nested Vec to Custom error, got:\n{pretty}"
        );
    }
}

// ============================================================================
// Help context propagation — Option<Vec<TaggedEnum>>
// ============================================================================

#[tpd]
#[derive(Debug, PartialEq, Eq)]
pub struct HcItem {
    value: u8,
}

impl VerifyIn<PartialHcContainer> for PartialHcItem {
    fn verify(&mut self, _parent: &PartialHcContainer) -> Result<(), ValidationFailure>
    where
        Self: Sized + toml_pretty_deser::Visitor,
    {
        if let Some(99) = self.value.as_ref() {
            self.value.state = TomlValueState::Custom {
                spans: vec![(self.value.span(), "Value cannot be 99".to_string())],
            };
        }
        if let Some(88) = self.value.as_ref() {
            return Err(ValidationFailure::new(
                "Value 88 is forbidden",
                Some("Use a different number"),
            ));
        }
        Ok(())
    }
}

#[tpd(tag = "kind")]
#[derive(Debug, PartialEq, Eq)]
pub enum HcTaggedEnum {
    Item(HcItem),
}

#[tpd(root, no_verify)]
#[derive(Debug)]
#[allow(dead_code)]
pub struct HcContainer {
    #[tpd(nested)]
    items: Option<Vec<HcTaggedEnum>>,
}

/// Help set on an `Option<Vec<TaggedEnum>>` field should propagate to errors
/// inside the vec elements.
#[test]
fn test_help_context_option_vec_tagged_enum() {
    let toml = r#"
        [[items]]
        kind = 'Item'
        value = 'not-a-number'
    "#;

    let result = HcContainer::tpd_from_toml(
        toml,
        toml_pretty_deser::FieldMatchMode::Exact,
        toml_pretty_deser::VecMode::Strict,
    );

    assert!(result.is_err(), "should fail: value has wrong type");

    if let Err(mut e) = result {
        if let DeserError::DeserFailure(_, ref mut partial) = e {
            if let Some(root) = partial.value.as_mut() {
                root.items.set_help("See: https://docs.example.com/items");
            }
        }

        let pretty = e.pretty("test.toml");
        insta::assert_snapshot!(pretty);
        assert!(
            pretty.contains("docs.example.com/items"),
            "expected items help to propagate to child errors, but got:\n{pretty}"
        );
    }
}
#[test]
fn test_help_context_option_vec_tagged_enum_custom() {
    let toml = r#"
        [[items]]
        kind = 'Item'
        value = 99
    "#;

    let result = HcContainer::tpd_from_toml(
        toml,
        toml_pretty_deser::FieldMatchMode::Exact,
        toml_pretty_deser::VecMode::Strict,
    );

    assert!(result.is_err(), "should fail: value has wrong type");

    if let Err(mut e) = result {
        if let DeserError::DeserFailure(_, ref mut partial) = e {
            if let Some(root) = partial.value.as_mut() {
                root.items.set_help("See: https://docs.example.com/items");
            }
        }

        let pretty = e.pretty("test.toml");
        insta::assert_snapshot!(pretty);
        assert!(
            pretty.contains("docs.example.com/items"),
            "expected items help to propagate to child errors, but got:\n{pretty}"
        );
    }
}

/// When `verify()` returns `Err(ValidationFailure)` (ValidationFailed path),
/// the "Involving this enum variant." context annotation must still appear.
#[test]
fn test_help_context_option_vec_tagged_enum_validation_failed() {
    let toml = r#"
        [[items]]
        kind = 'Item'
        value = 88
    "#;

    let result = HcContainer::tpd_from_toml(
        toml,
        toml_pretty_deser::FieldMatchMode::Exact,
        toml_pretty_deser::VecMode::Strict,
    );

    assert!(result.is_err(), "should fail: value 88 is forbidden");

    if let Err(mut e) = result {
        if let DeserError::DeserFailure(_, ref mut partial) = e {
            if let Some(root) = partial.value.as_mut() {
                root.items.set_help("See: https://docs.example.com/items");
            }
        }

        let pretty = e.pretty("test.toml");
        insta::assert_snapshot!(pretty);
        assert!(
            pretty.contains("docs.example.com/items"),
            "expected items help to propagate to child errors, but got:\n{pretty}"
        );
        assert!(
            pretty.contains("Involving this enum variant."),
            "expected enum context annotation, but got:\n{pretty}"
        );
    }
}


//try to expose acontext bug
//
//
#[tpd(no_verify)]
#[allow(dead_code)]
pub struct Inner {
    a: u8,
}
#[tpd(tag = "kind")]
pub enum InnerTagged {
    A(Inner),
    B(Inner),
}
#[tpd(root)]
#[allow(dead_code)]
pub struct Outer {
    #[tpd(nested)]
    inner: Option<Vec<InnerTagged>>,
}

impl VerifyIn<TPDRoot> for PartialOuter {
    fn verify(&mut self, _parent: &TPDRoot) -> Result<(), ValidationFailure>
    where
        Self: Sized + toml_pretty_deser::Visitor,
    {
        if let Some(Some(inner)) = self.inner.as_mut() {
            for item in inner.iter_mut() {
                item.state = TomlValueState::Custom {
                    spans: vec![(item.span(), "Forced custom error".to_string())],
                };
                item.help = Some("help here".to_string());
            }
        }
        Ok(())
    }
}

#[test]
fn test_context_bug() {
    let toml = "
        [[inner]]
            kind = 'A'
            a = 23
    ";
    let result = Outer::tpd_from_toml(
        toml,
        toml_pretty_deser::FieldMatchMode::Exact,
        toml_pretty_deser::VecMode::Strict,
    );

    assert!(result.is_err(), "should fail: value has wrong type");

    if let Err(e) = result {
        if let DeserError::DeserFailure(_, ref _partial) = e {
            let pretty = e.pretty("test.toml");
            assert!(pretty.contains("Involving this enum variant."));
            insta::assert_snapshot!(pretty);
        }
    }
}

// ============================================================================
// Context annotation on Vec<Nested>
// ============================================================================

#[tpd(no_verify)]
#[derive(Debug)]
#[allow(dead_code)]
pub struct CtxVecItem {
    value: u8,
    label: String,
}

#[tpd(root, no_verify)]
#[derive(Debug)]
#[allow(dead_code)]
pub struct CtxVecRoot {
    #[tpd(nested)]
    items: Vec<CtxVecItem>,
}

/// `set_context` on a `Vec<Nested>` field attaches a secondary source-span
/// annotation to every descendant error, pointing at the array.
/// This demonstrates the generalised context mechanism: any `TomlValue` at any
/// nesting level can carry a context label — not just tagged enums.
#[test]
fn test_context_on_vec_nested() {
    let toml = r#"
        [[items]]
        value = 999
        label = 'first'

        [[items]]
        value = 12
        label = 300
    "#;

    let result = CtxVecRoot::tpd_from_toml(toml, FieldMatchMode::Exact, VecMode::Strict);
    assert!(result.is_err());

    if let Err(mut e) = result {
        if let DeserError::DeserFailure(_, ref mut partial) = e {
            if let Some(root) = partial.value.as_mut() {
                root.items.set_context("In the 'items' list");
            }
        }
        let pretty = e.pretty("test.toml");
        assert!(
            pretty.contains("In the 'items' list"),
            "context annotation must appear as a source span, got:\n{pretty}"
        );
        insta::assert_snapshot!(pretty);
    }
}
