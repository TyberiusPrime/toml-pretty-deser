// Regression test: adapt_in_verify(R) on a plain Vec<T> field should generate
// per-element wrapping — Vec<TomlValue<MustAdapt<R, T>>> — not a whole-vec wrap
// MustAdapt<R, Vec<T>>.
//
// Without the fix the macro emits
//   paths: TomlValue<MustAdapt<String, Vec<PathBuf>>>
// so `self.paths.value` holds a MustAdapt<String, Vec<PathBuf>> which has no
// way to iterate elements, and the adapt() call in verify operates on the whole
// Vec<PathBuf> at once rather than element-by-element.
//
// With the correct fix the partial field type is
//   TomlValue<Vec<TomlValue<MustAdapt<String, PathBuf>>>>
// mirroring what already works for Option<Vec<T>>.

use std::path::PathBuf;
use toml_pretty_deser::prelude::*;

#[tpd(root)]
#[derive(Debug)]
struct Config {
    /// Required (non-optional) Vec, each element adapted String → PathBuf in verify.
    #[tpd(adapt_in_verify(String))]
    paths: Vec<PathBuf>,
}

impl VerifyIn<TPDRoot> for PartialConfig {
    fn verify(
        &mut self,
        _parent: &TPDRoot,
        _options: &VerifyOptions,
    ) -> Result<(), ValidationFailure> {
        // Type assertion: inner value must be Vec<TomlValue<MustAdapt<String, PathBuf>>>,
        // NOT MustAdapt<String, Vec<PathBuf>>.
        let _: &Vec<TomlValue<MustAdapt<String, PathBuf>>> =
            self.paths.value.as_ref().unwrap();

        for element in self.paths.value.as_mut().unwrap().iter_mut() {
            element.adapt(|s: String| (PathBuf::from(s), TomlValueState::Ok));
        }
        Ok(())
    }
}

#[test]
fn vec_adapt_in_verify_converts_elements() {
    let toml = r#"paths = ["/tmp/foo", "/tmp/bar"]"#;
    let config = Config::tpd_from_toml(toml, FieldMatchMode::Exact, VecMode::Strict).unwrap();
    assert_eq!(config.paths.len(), 2);
    assert_eq!(config.paths[0], PathBuf::from("/tmp/foo"));
    assert_eq!(config.paths[1], PathBuf::from("/tmp/bar"));
}

#[test]
fn vec_adapt_in_verify_wrong_element_type_reports_error() {
    // Elements must be strings; integers should fail.
    let toml = r#"paths = [42, 99]"#;
    let result = Config::tpd_from_toml(toml, FieldMatchMode::Exact, VecMode::Strict);
    assert!(
        result.is_err(),
        "integer elements should fail for String intermediate type"
    );
    let pretty = result.unwrap_err().pretty("test.toml");
    assert!(
        pretty.contains("string"),
        "error should mention expected type, got:\n{pretty}"
    );
}
