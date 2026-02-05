use toml_pretty_deser::prelude::*;
#[derive(Debug, Clone, PartialEq, Eq)]
struct DNA(String);

impl PartialEq<&str> for DNA {
    fn eq(&self, other: &&str) -> bool {
        &self.0 == *other
    }
}

#[tpd_make_partial]
#[derive(Debug)]
struct BarcodesValidated {
    barcodes: Vec<DNA>,
}

impl FromTomlItem for DNA {
    fn from_toml_item(
        item: &toml_edit::Item,
        parent_span: std::ops::Range<usize>,
        _col: &TomlCollector,
    ) -> TomlValue<DNA> {
        match item.as_str() {
            Some(s) => {
                if s.chars()
                    .all(|c| matches!(c, 'a' | 'c' | 'g' | 't' | 'A' | 'C' | 'G' | 'T'))
                {
                    TomlValue::new_ok(DNA(s.to_string()), parent_span)
                } else {
                    TomlValue::new_validation_failed(
                        item.span().unwrap_or(parent_span),
                        "Invalid base".to_string(),
                        Some("Use only AGTC".to_string()),
                    )
                }
            }
            None => TomlValue::new_wrong_type(item, parent_span, "String(DNA)"),
        }
    }
}

#[test]
fn test_vec_validate_elements() {
    let toml = "
        barcodes = [ 'agtc', 'ccGc']
    ";
    let result: Result<_, _> = deserialize::<PartialBarcodesValidated, BarcodesValidated>(toml);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.barcodes.len(), 2);
        assert_eq!(output.barcodes, ["agtc", "ccGc"]);
    }
}

#[tpd_make_partial]
#[derive(Debug)]
struct BarcodesValidatedSingle {
    barcodes: Vec<DNA>,
}

#[test]
fn test_vec_allow_single() {
    let toml = "
        barcodes = 'agtc'
    ";
    let result: Result<_, _> =
        deserialize_with_mode::<PartialBarcodesValidatedSingle, BarcodesValidatedSingle>(toml,
        FieldMatchMode::Exact, VecMode::SingleOk
    );
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.barcodes.len(), 1);
        assert_eq!(output.barcodes, ["agtc"]);
    }
}