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
    barcode: Option<DNA>,
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
fn test_opt_validated() {
    let toml = "
        barcode = 'agtc'
    ";
    let result: Result<_, _> = deserialize::<PartialBarcodesValidated, BarcodesValidated>(toml);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.barcode, Some(DNA("agtc".to_string())));
    }
}
