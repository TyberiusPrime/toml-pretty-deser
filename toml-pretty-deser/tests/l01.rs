// Test: adapt_in_verify(R) on Option<Vec<T>> generates per-element MustAdapt wrapping.
//
// The partial field type becomes TomlValue<Option<Vec<TomlValue<MustAdapt<R, T>>>>>
// so that in verify() the user receives an Option<Vec<MustAdapt<R, T>>> and can
// iterate, adapting each element individually.

use std::path::PathBuf;
use toml_pretty_deser::prelude::*;

#[tpd(root)]
#[derive(Debug)]
struct Config {
    /// Deserialised as Vec<String>, each element converted to PathBuf in verify.
    #[tpd(adapt_in_verify(String))]
    paths: Option<Vec<PathBuf>>,
}

impl VerifyIn<TPDRoot> for PartialConfig {
    fn verify(
        &mut self,
        _parent: &TPDRoot,
        _options: &VerifyOptions,
    ) -> Result<(), ValidationFailure> {
        // Type assertion: the inner value is exactly Option<Vec<TomlValue<MustAdapt<String, PathBuf>>>>
        let _: &Option<Vec<TomlValue<MustAdapt<String, PathBuf>>>> =
            self.paths.value.as_ref().unwrap();

        // Adapt each element from String → PathBuf
        if let Some(Some(items)) = self.paths.value.as_mut() {
            for element in items.iter_mut() {
                element.adapt(|s: String| (PathBuf::from(s), TomlValueState::Ok));
            }
        }
        Ok(())
    }
}

#[test]
fn option_vec_adapt_in_verify_with_values() {
    let toml = r#"paths = ["/tmp/foo", "/tmp/bar"]"#;
    let config = Config::tpd_from_toml(toml, FieldMatchMode::Exact, VecMode::Strict).unwrap();
    let paths = config.paths.unwrap();
    assert_eq!(paths.len(), 2);
    assert_eq!(paths[0], PathBuf::from("/tmp/foo"));
    assert_eq!(paths[1], PathBuf::from("/tmp/bar"));
}

#[test]
fn option_vec_adapt_in_verify_absent_is_none() {
    let toml = "";
    let config = Config::tpd_from_toml(toml, FieldMatchMode::Exact, VecMode::Strict).unwrap();
    assert!(config.paths.is_none());
}

#[test]
fn option_vec_adapt_in_verify_wrong_element_type_reports_error() {
    // elements must be strings (adapt_in_verify(String)); integers should fail
    let toml = r#"paths = [42, 99]"#;
    let result = Config::tpd_from_toml(toml, FieldMatchMode::Exact, VecMode::Strict);
    assert!(result.is_err(), "integer elements should fail for String intermediate type");
    let pretty = result.unwrap_err().pretty("test.toml");
    assert!(pretty.contains("string"), "error should mention expected type, got:\n{pretty}");
}
