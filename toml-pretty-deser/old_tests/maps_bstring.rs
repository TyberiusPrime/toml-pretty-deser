//! Test IndexMap with BString keys from the bstr crate.
//!
//! This tests that IndexMap works with key types other than String.

use bstr::BString;
use indexmap::IndexMap;
use toml_pretty_deser::prelude::*;

#[tpd]
#[derive(Debug)]
struct BStringMapped {
    items: IndexMap<BString, String>,
}

#[test]
fn test_indexmap_bstring_key_happy() {
    let toml = r#"
        [items]
        alpha = "first"
        beta = "second"
        gamma = "third"
    "#;
    let result: Result<_, _> = deserialize::<PartialBStringMapped, BStringMapped>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.items.len(), 3);
        assert_eq!(
            output.items.get(&BString::from("alpha")),
            Some(&"first".to_string())
        );
        assert_eq!(
            output.items.get(&BString::from("beta")),
            Some(&"second".to_string())
        );
        assert_eq!(
            output.items.get(&BString::from("gamma")),
            Some(&"third".to_string())
        );

        // Verify order is preserved
        let keys: Vec<&BString> = output.items.keys().collect();
        assert_eq!(keys[0], &BString::from("alpha"));
        assert_eq!(keys[1], &BString::from("beta"));
        assert_eq!(keys[2], &BString::from("gamma"));
    }
}

#[test]
fn test_indexmap_bstring_key_empty() {
    let toml = r#"
        [items]
    "#;
    let result: Result<_, _> = deserialize::<PartialBStringMapped, BStringMapped>(toml);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.items.len(), 0);
    }
}

#[tpd]
#[derive(Debug)]
struct OptionalBStringMapped {
    items: Option<IndexMap<BString, String>>,
}

#[test]
fn test_indexmap_bstring_key_optional_present() {
    let toml = r#"
        [items]
        foo = "bar"
    "#;
    let result: Result<_, _> =
        deserialize::<PartialOptionalBStringMapped, OptionalBStringMapped>(toml);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert!(output.items.is_some());
        let items = output.items.unwrap();
        assert_eq!(items.get(&BString::from("foo")), Some(&"bar".to_string()));
    }
}

#[test]
fn test_indexmap_bstring_key_optional_missing() {
    let toml = r#"
    "#;
    let result: Result<_, _> =
        deserialize::<PartialOptionalBStringMapped, OptionalBStringMapped>(toml);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert!(output.items.is_none());
    }
}

#[tpd]
#[derive(Debug)]
struct BStringMappedVec {
    items: IndexMap<BString, Vec<String>>,
}

#[test]
fn test_indexmap_bstring_key_with_vec_values() {
    let toml = r#"
        [items]
        fruits = ["apple", "banana", "cherry"]
        colors = ["red", "green", "blue"]
    "#;
    let result: Result<_, _> = deserialize::<PartialBStringMappedVec, BStringMappedVec>(toml);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.items.len(), 2);
        assert_eq!(
            output.items.get(&BString::from("fruits")),
            Some(&vec![
                "apple".to_string(),
                "banana".to_string(),
                "cherry".to_string()
            ])
        );
        assert_eq!(
            output.items.get(&BString::from("colors")),
            Some(&vec![
                "red".to_string(),
                "green".to_string(),
                "blue".to_string()
            ])
        );
    }
}

#[tpd]
#[derive(Debug, Clone, PartialEq, Eq)]
enum Status {
    Active,
    Inactive,
    Pending,
}

#[tpd]
#[derive(Debug)]
struct BStringMappedEnum {
    statuses: IndexMap<BString, Status>,
}

#[test]
fn test_indexmap_bstring_key_with_enum_values() {
    let toml = r#"
        [statuses]
        user1 = "Active"
        user2 = "Inactive"
        user3 = "Pending"
    "#;
    let result: Result<_, _> = deserialize::<PartialBStringMappedEnum, BStringMappedEnum>(toml);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.statuses.len(), 3);
        assert_eq!(
            output.statuses.get(&BString::from("user1")),
            Some(&Status::Active)
        );
        assert_eq!(
            output.statuses.get(&BString::from("user2")),
            Some(&Status::Inactive)
        );
        assert_eq!(
            output.statuses.get(&BString::from("user3")),
            Some(&Status::Pending)
        );
    }
}
