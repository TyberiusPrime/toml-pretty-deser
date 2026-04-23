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
    #[tpd(nested)]
    output: Option<Output>,
}

#[derive(Debug)]
#[tpd]
struct Output {

    #[tpd(default, alias = "hello")]
    interleave: Option<Vec<String>>,

}

impl VerifyIn<TPDRoot> for PartialConfig {
    fn verify(
        &mut self,
        _parent: &TPDRoot,
        _options: &VerifyOptions,
    ) -> Result<(), ValidationFailure> {
        Ok(())
    }
}

impl VerifyIn<PartialConfig> for PartialOutput {
    fn verify(
        &mut self,
        _parent: &PartialConfig,
        _options: &VerifyOptions,
    ) -> Result<(), ValidationFailure> {
        Ok(())
    }
}

/// When a required field is missing AND verify fails, the missing-key error
/// must not carry the ValidationFailure's help text.
#[test]
fn wrong_value_option_vec_string_single_ok_error_message() {
    // `name` is missing; `value = 0` triggers the ValidationFailure in verify.
    let toml = "
[output]
    interleave = true
";
    let result = Config::tpd_from_toml(toml, FieldMatchMode::Exact, VecMode::SingleOk);
    assert!(result.is_err());
    let pretty = result.unwrap_err().pretty("test.toml");
    insta::assert_snapshot!(pretty);
}
