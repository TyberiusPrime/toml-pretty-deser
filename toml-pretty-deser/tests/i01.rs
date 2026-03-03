// Regression test: context annotation set on a Custom-state TomlValue (from
// within VerifyIn) is accessible on the DeserError but does NOT appear in the
// .pretty() output as a span on the Custom error itself.
//
// Root cause (lib.rs line 1224, v_context_spans / register_error_with_context):
//   The Custom arm of `register_error_with_context` pushes `self.context` into
//   the collector only for *child* errors — it never attaches it as an extra
//   span on the Custom error itself.  When the children are all Ok (TOML parsed
//   fine, error was injected purely by VerifyIn), the context is pushed and
//   immediately popped without any child error consuming it, so the label never
//   reaches the rendered output.
//
// The test below demonstrates both observations:
//   a) `items.context.is_some()` — PASSES: the annotation is stored and
//      accessible between receiving the DeserError and calling .pretty().
//   b) `pretty.contains("Validation triggered at items")` — FAILS: the
//      annotation is silently discarded from the Custom error's own output.

#[allow(clippy::must_use_candidate)]
use toml_pretty_deser::prelude::*;
use toml_pretty_deser::{TPDRoot, TomlValueState};
use toml_pretty_deser_macros::tpd;

// ─── structs under test ───────────────────────────────────────────────────────

#[tpd(no_verify)]
#[derive(Debug)]
#[allow(dead_code)]
pub struct CvItem {
    value: u32,
}

#[tpd(root)]
#[derive(Debug)]
#[allow(dead_code)]
pub struct CvRoot {
    #[tpd(nested)]
    items: Option<Vec<CvItem>>,
}

impl VerifyIn<TPDRoot> for PartialCvRoot {
    fn verify(
        &mut self,
        _parent: &TPDRoot,
        _options: &VerifyOptions,
    ) -> Result<(), ValidationFailure>
    where
        Self: Sized + toml_pretty_deser::Visitor,
    {
        // Set the outer items TomlValue to Custom state and attach a context
        // annotation.  The annotation is intended to appear as a span on the
        // Custom error in .pretty() output.
        let span = self.items.span.clone();
        self.items.state = TomlValueState::Custom {
            spans: vec![(span.clone(), "items list failed validation".to_string())],
        };
        self.items
            .set_context_at(span, "Validation triggered at items");
        Ok(())
    }
}

// ─── tests ────────────────────────────────────────────────────────────────────

/// VerifyIn sets the outer `items` TomlValue to Custom state and attaches a
/// context annotation via `set_context_at`.  The TOML itself is valid (all
/// items parse correctly), so the only source of errors is the Custom state
/// injected by VerifyIn.
///
/// Assertion (a): context IS accessible on the DeserError before .pretty().
/// Assertion (b): context annotation appears in .pretty() output on the Custom
///   error — this currently FAILS, exposing the bug described above.
#[test]
fn test_context_on_custom_state_accessible_and_rendered() {
    let toml = r#"
        [[items]]
        value = 1

        [[items]]
        value = 2
    "#;

    let result = CvRoot::tpd_from_toml(toml, FieldMatchMode::Exact, VecMode::Strict);
    assert!(result.is_err(), "VerifyIn should mark items as invalid");

    if let Err(mut e) = result {
        if let DeserError::DeserFailure(_, ref mut partial) = e {
            if let Some(root) = partial.value.as_mut() {
                // (a) The context annotation set during VerifyIn is stored on
                // `items.context` and is accessible here, before .pretty() is
                // called — even though it was set "inside" VerifyIn.
                assert!(
                    root.items.context.is_some(),
                    "context set during VerifyIn should be accessible on the DeserError \
                     before .pretty() is called"
                );
            }
        }

        let pretty = e.pretty("test.toml");

        // (b) The context label must appear as a span annotation on the Custom
        // error for `items` itself.  Currently it does NOT — the Custom arm of
        // `register_error_with_context` only pushes `self.context` for child
        // errors, so when every child is Ok the label is never consumed.
        assert!(
            pretty.contains("Validation triggered at items"),
            "context set during VerifyIn should appear in .pretty() output on the \
             Custom error, got:\n{pretty}"
        );

        insta::assert_snapshot!(pretty);
    }
}

// ============================================================================
// Tagged-enum context override between DeserError and .pretty()
// ============================================================================
//
// For a tagged-enum element in Nested state (a genuine parse error inside one
// of the variant's inner structs), fill_from_toml already calls
// `.with_context_at(tag_span, "Involving this enum variant.")` eagerly (macro
// line 1187), and tpd_validate preserves that annotation through the Nested
// arm.  This means the user CAN inspect and overwrite it between receiving the
// DeserError and calling .pretty() — unlike the v_context_spans mechanism
// (line 1224) which is only invoked for Custom-state values inside .pretty()
// and therefore cannot be modified by callers.

#[tpd(no_verify)]
#[derive(Debug)]
#[allow(dead_code)]
pub struct StepResize {
    width: u32,
}

#[tpd(tag = "kind")]
#[derive(Debug)]
pub enum PipelineStep {
    Resize(StepResize),
}

#[tpd(root)]
#[derive(Debug)]
#[allow(dead_code)]
pub struct CvPipeline {
    #[tpd(nested)]
    transform: Vec<PipelineStep>,
}

impl VerifyIn<TPDRoot> for PartialCvPipeline {
    fn verify(
        &mut self,
        _parent: &TPDRoot,
        _options: &VerifyOptions,
    ) -> Result<(), ValidationFailure>
    where
        Self: Sized + toml_pretty_deser::Visitor,
    {
        // Phase 1: collect (index, span) for elements where width == 55.
        let bad: Vec<(usize, std::ops::Range<usize>)> = self
            .transform
            .value
            .as_ref()
            .map(|steps| {
                steps
                    .iter()
                    .enumerate()
                    .filter_map(|(i, tv_step)| {
                        if let Some(step) = tv_step.as_ref() {
                            if let PartialPipelineStep::Resize(e) = step {
                                let resize = &e.toml_value;
                                if let Some(resize) = resize.as_ref() {
                                    if *resize.width.as_ref().unwrap() == 55 {
                                        return Some((i, tv_step.span()));
                                    }
                                }
                            }
                        }
                        None
                    })
                    .collect()
            })
            .unwrap_or_default();

        // Phase 2: set each bad element's state to Custom directly.
        if let Some(steps) = self.transform.value.as_mut() {
            for (i, span) in bad {
                steps[i].state = TomlValueState::Custom {
                    spans: vec![(span, "width cannot be 55".to_string())],
                };
            }
        }
        Ok(())
    }
}

/// Two pipeline steps: the first is valid, the second has a wrong-type field.
/// Between the DeserError and .pretty() the caller iterates the vec, overwrites
/// the default "Involving this enum variant." context label with "In this step",
/// and attaches a doc-URL help string keyed on the variant name.
///
/// Both the overwritten label and the help URL must appear in .pretty() output,
/// demonstrating that Nested-state tagged-enum contexts are eagerly stored and
/// fully modifiable before rendering — in contrast to Custom-state contexts
/// (test above) where the annotation is silently dropped.
#[test]
fn test_tagged_enum_context_overwrite_between_deser_and_pretty() {
    let toml = r#"
        [[transform]]
        kind = "Resize"
        width = 200

        [[transform]]
        kind = "Resize"
        width = "not-a-number"
    "#;

    let result = CvPipeline::tpd_from_toml(toml, FieldMatchMode::Exact, VecMode::Strict);
    assert!(result.is_err(), "second step has wrong type for width");

    let doc_url = "https://docs.example.com/steps/";

    if let Err(mut e) = result {
        if let DeserError::DeserFailure(_, ref mut tv_partial) = e {
            if let Some(partial) = tv_partial.value.as_mut()
                && let Some(steps) = partial.transform.value.as_mut()
            {
                for tv_step in steps.iter_mut() {
                    if !tv_step.is_ok()
                        && let Some(step) = tv_step.value.as_ref()
                    {
                        let step_name = step.tpd_get_tag();
                        tv_step.help = Some(format!("See {doc_url}{step_name}"));
                        if let Some(context) = tv_step.context.as_mut() {
                            context.1 = "In this step".to_string();
                        }
                    }
                }
            }
        }

        let pretty = e.pretty("test.toml");

        // The overwritten context label appears as a secondary span annotation
        // on the width wrong-type error.
        assert!(
            pretty.contains("In this step"),
            "overwritten context label should appear in output, got:\n{pretty}"
        );
        // The original default label must be gone — it was replaced, not appended.
        assert!(
            !pretty.contains("Involving this enum variant."),
            "original default label should be replaced, got:\n{pretty}"
        );
        // The help URL (with the variant name interpolated) must also appear.
        assert!(
            pretty.contains(&format!("{doc_url}Resize")),
            "doc-URL help should appear in output, got:\n{pretty}"
        );

        insta::assert_snapshot!(pretty);
    }
}

#[test]
fn test_tagged_enum_context_overwrite_between_deser_and_pretty_custom() {
    let toml = r#"
        [[transform]]
        kind = "Resize"
        width = 200

        [[transform]]
        kind = "Resize"
        width = 55
    "#;

    let result = CvPipeline::tpd_from_toml(toml, FieldMatchMode::Exact, VecMode::Strict);
    assert!(result.is_err());

    let doc_url = "https://docs.example.com/steps/";

    if let Err(mut e) = result {
        if let DeserError::DeserFailure(_, ref mut tv_partial) = e {
            if let Some(partial) = tv_partial.value.as_mut()
                && let Some(steps) = partial.transform.value.as_mut()
            {
                for tv_step in steps.iter_mut() {
                    if !tv_step.is_ok()
                        && let Some(step) = tv_step.value.as_ref()
                    {
                        let step_name = step.tpd_get_tag();
                        tv_step.help = Some(format!("See {doc_url}{step_name}"));
                        if let Some(context) = tv_step.context.as_mut() {
                            context.1 = "In this step!".to_string();
                        }
                    }
                }
            }
        }

        let pretty = e.pretty("test.toml");
        insta::assert_snapshot!(pretty);
        assert!(pretty.contains("In this step"));
    }
}
