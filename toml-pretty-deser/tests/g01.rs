// Regression test: help text is swallowed when setting TomlValueState::Custom
// (with help) directly on keys inside a MapAndKeys for an
// Option<IndexMap<String, T>> field.
//
// The scenario being tested:
//
//   1. The field is `barcodes: Option<IndexMap<String, u8>>`.
//   2. In `verify`, we access the keys directly (there is no `FailableKeys`
//      impl for `TomlValue<Option<MapAndKeys<K, V>>>`, only for the unwrapped
//      `TomlValue<MapAndKeys<K, V>>`).
//   3. We set `TomlValueState::Custom` + `help` on every key.
//   4. The snapshot should contain a "Hint: …" line — if it does not, the help
//      is being swallowed.

use indexmap::IndexMap;
use toml_pretty_deser::prelude::*;
use toml_pretty_deser::{TPDRoot, TomlValueState};
use toml_pretty_deser_macros::tpd;

// ─── struct under test ───────────────────────────────────────────────────────

#[tpd(root)]
#[derive(Debug)]
#[allow(dead_code)]
pub struct BarcodeConfig {
    barcodes: Option<IndexMap<String, u8>>,
}

impl VerifyIn<TPDRoot> for PartialBarcodeConfig {
    fn verify(&mut self, _parent: &TPDRoot) -> Result<(), ValidationFailure>
    where
        Self: Sized + toml_pretty_deser::Visitor,
    {
        // Access keys of the Option<IndexMap<…>> field directly.
        // (FailableKeys is only impl'd for TomlValue<MapAndKeys<K,V>>,
        //  not for TomlValue<Option<MapAndKeys<K,V>>>, so we must unwrap.)
        if let Some(opt_map) = self.barcodes.value.as_mut() {
            if let Some(map_and_keys) = opt_map.as_mut() {
                for key in &mut map_and_keys.keys {
                    let span = key.span.clone();
                    key.state = TomlValueState::Custom {
                        spans: vec![(span, "This key is not allowed".to_string())],
                    };
                    // ← this help should appear as "Hint: …" in the output
                    key.help = Some("All keys are forbidden in this test".to_string());
                }
            }
        }
        Ok(())
    }
}

// ─── tests ───────────────────────────────────────────────────────────────────

/// All map values are valid u8 → field is Ok before verify runs → as_mut()
/// succeeds → key Custom errors are set → hint should appear.
#[test]
fn test_map_key_custom_help_values_ok() {
    let toml = r#"
[barcodes]
foo = 1
bar = 2
"#;
    let result: Result<_, _> =
        BarcodeConfig::tpd_from_toml(toml, FieldMatchMode::Exact, VecMode::Strict);
    assert!(!result.is_ok(), "verify marks all keys invalid");
    if let Err(e) = result {
        assert!(e.pretty("test.toml").contains("Hint: All keys are forbidden in this test"));
        insta::assert_snapshot!(e.pretty("test.toml"));
    }
}

/// One map value has wrong type (string instead of u8) → MapAndKeys gets
/// Nested during fill_from_toml → Option::fill_from_toml calls
/// convert_failed_type() which DROPS the inner value (value = None) →
/// field arrives in verify as Nested with value = None → as_mut() returns
/// None → key Custom error setup is silently skipped → no key errors and no
/// hint shown, even though the verify code tries to set them.
#[test]
fn test_map_key_custom_help_values_have_errors() {
    let toml = r#"
[barcodes]
foo = "not_a_number"
bar = 2
"#;
    let result: Result<_, _> =
        BarcodeConfig::tpd_from_toml(toml, FieldMatchMode::Exact, VecMode::Strict);
    assert!(!result.is_ok(), "foo has wrong type so parsing fails");
    if let Err(e) = result {
        // Snapshot will show the wrong-type error for foo's VALUE.
        // It will NOT show any key errors with hints — the key Custom code
        // in verify was silently skipped because as_mut() returned None
        // (field was already Nested when verify ran).
        insta::assert_snapshot!(e.pretty("test.toml"));
    }
}
