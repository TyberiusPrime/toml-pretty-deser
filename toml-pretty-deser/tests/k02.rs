// Regression test: Option<IndexMap<K, V>> with #[tpd(nested)] where K: TryFrom<String>.
// Matches the user's reported pattern:
//   #[tpd(nested)]
//   barcodes: Option<IndexMap<TagLabel, Barcodes>>

use indexmap::IndexMap;
use toml_pretty_deser::prelude::*;

/// Key type with TryFrom<String> that can fail.
#[derive(Debug, PartialEq, Eq, Hash)]
struct TagLabel(String);

impl TryFrom<&str> for TagLabel {
    type Error = String;

    fn try_from(s: &str) -> Result<Self, Self::Error> {
        if s.starts_with('-') || s.is_empty() {
            Err(format!(
                "tag label must not be empty or start with '-', got {:?}",
                s
            ))
        } else {
            Ok(TagLabel(s.to_string()))
        }
    }
}

#[derive(Debug, Hash, Eq, PartialEq)]
struct BString(String);
//
// impl From<String> for BString {
//     fn from(value: String) -> Self {
//         BString(value)
//     }
// }

impl From<&str> for BString {
    fn from(value: &str) -> Self {
        BString(value.to_string())
    }
}

/// Bog-standard nested struct.
#[tpd(no_verify)]
#[derive(Debug)]
pub struct Barcodes {
    // #[serde(
    //     deserialize_with = "deser::btreemap_iupac_dna_string_from_string",
    //     flatten
    // )]
    #[tpd(absorb_remaining)]
    pub barcode_to_name: IndexMap<BString, String>,
}

#[tpd(root, no_verify)]
#[derive(Debug)]
struct Root {
    #[tpd(nested)]
    barcodes: Option<IndexMap<TagLabel, Barcodes>>,
}

// --- success cases ---

#[test]
fn nested_option_map_absent_is_none() {
    let toml = "";
    let root = Root::tpd_from_toml(toml, FieldMatchMode::Exact, VecMode::Strict).unwrap();
    assert!(root.barcodes.is_none());
}

#[test]
fn nested_option_map_valid_keys_parse_ok() {
    let toml = r#"
[barcodes.ean13]
value = "1234567890123"
value2 = "4"

[barcodes.qr]
value = "hello"
"#;
    let root = Root::tpd_from_toml(toml, FieldMatchMode::Exact, VecMode::Strict).unwrap();
    let barcodes = root.barcodes.unwrap();
    assert_eq!(barcodes.len(), 2);
    assert_eq!(
        barcodes[&TagLabel("ean13".into())]
            .barcode_to_name
            .get(&BString::from("value")),
        Some(&"1234567890123".to_string())
    );

    assert_eq!(
        barcodes[&TagLabel("ean13".into())]
            .barcode_to_name
            .get(&BString::from("value2")),
        Some(&"4".to_string())
    );
}

// --- failure cases ---

#[test]
fn nested_option_map_invalid_key_gives_validation_error() {
    let toml = r#"
[barcodes.valid]
value = "ok"

[barcodes.-bad]
value = "broken"
"#;
    let result = Root::tpd_from_toml(toml, FieldMatchMode::Exact, VecMode::Strict);
    assert!(result.is_err(), "invalid tag label key should fail");
    let pretty = result.unwrap_err().pretty("test.toml");
    assert!(
        pretty.contains("must not be empty or start with"),
        "TryFrom error must appear in output for nested Option<IndexMap>, got:\n{pretty}"
    );
    insta::assert_snapshot!(pretty);
}

#[test]
fn nested_option_map_value_parse_error_is_reported() {
    let toml = r#"
[barcodes.good]
value = "ok"

[barcodes.broken]
value = 42
"#;
    let result = Root::tpd_from_toml(toml, FieldMatchMode::Exact, VecMode::Strict);
    assert!(
        result.is_err(),
        "wrong-type value inside nested map should fail"
    );
    let pretty = result.unwrap_err().pretty("test.toml");
    assert!(
        pretty.contains("string"),
        "value type error should be reported, got:\n{pretty}"
    );
    insta::assert_snapshot!(pretty);
}
