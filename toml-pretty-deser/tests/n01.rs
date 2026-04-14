// Regression test: when a struct is missing a required key AND verify() returns
// Err(ValidationFailure) with a help string, the ValidationFailure's help text
// should NOT be appended to the "Missing required key" error.
//
// The bug: register_error_with_context for ValidationFailed pushes self.help into
// col.context_help BEFORE calling value.v_register_errors(col), so the Missing-key
// error inside the struct picks up the ValidationFailure help as extra context.

use toml_pretty_deser::prelude::*;

#[tpd(root)]
#[derive(Debug)]
#[allow(dead_code)]
struct Config {
    name: String,
    value: u8,
}

impl VerifyIn<TPDRoot> for PartialConfig {
    fn verify(
        &mut self,
        _parent: &TPDRoot,
        _options: &VerifyOptions,
    ) -> Result<(), ValidationFailure> {
        if let Some(v) = self.value.as_ref() {
            if *v == 0 {
                return Err(ValidationFailure::new(
                    "value must be non-zero",
                    Some("Pick any positive number."),
                ));
            }
        }
        Ok(())
    }
}

/// When a required field is missing AND verify fails, the missing-key error
/// must not carry the ValidationFailure's help text.
#[test]
fn missing_key_does_not_inherit_verify_help() {
    // `name` is missing; `value = 0` triggers the ValidationFailure in verify.
    let toml = "value = 0";
    let result = Config::tpd_from_toml(toml, FieldMatchMode::Exact, VecMode::Strict);
    assert!(result.is_err());
    let pretty = result.unwrap_err().pretty("test.toml");
    insta::assert_snapshot!(pretty);
}
