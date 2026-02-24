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
// Help context propagation — Option<Vec<TaggedEnum>>
// ============================================================================

#[tpd(no_verify)]
#[derive(Debug, PartialEq, Eq)]
pub struct HcItem {
    value: u8,
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
