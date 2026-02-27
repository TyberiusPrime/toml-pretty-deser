// Tests for #[tpd(with = "func")] on Option<Vec<T>> and Option<IndexMap<K, V>> fields.
//
// The adapter should see the inner element/value type, not the whole container.

use indexmap::IndexMap;
use toml_pretty_deser::prelude::*;

#[derive(Debug, PartialEq)]
struct Wrapped(String);

fn wrap(input: TomlValue<String>) -> TomlValue<Wrapped> {
    input.map(Wrapped)
}

// --- Option<Vec<T>> ---

#[tpd(root, no_verify)]
#[derive(Debug)]
struct WithOptionVec {
    #[tpd(with = "wrap")]
    tags: Option<Vec<Wrapped>>,
    name: String,
}

#[test]
fn test_option_vec_with_present() {
    let toml_str = r#"
name = "test"
tags = ["foo", "bar", "baz"]
"#;
    let parsed =
        WithOptionVec::tpd_from_toml(toml_str, FieldMatchMode::Exact, VecMode::Strict).unwrap();
    assert_eq!(parsed.name, "test");
    let tags = parsed.tags.expect("tags should be Some");
    assert_eq!(tags.len(), 3);
    assert_eq!(tags[0].0, "foo");
    assert_eq!(tags[1].0, "bar");
    assert_eq!(tags[2].0, "baz");
}

#[test]
fn test_option_vec_with_absent() {
    let toml_str = r#"name = "test""#;
    let parsed =
        WithOptionVec::tpd_from_toml(toml_str, FieldMatchMode::Exact, VecMode::Strict).unwrap();
    assert_eq!(parsed.name, "test");
    assert!(parsed.tags.is_none());
}

// --- Option<IndexMap<K, V>> ---

#[tpd(root, no_verify)]
#[derive(Debug)]
struct WithOptionMap {
    #[tpd(with = "wrap")]
    entries: Option<IndexMap<String, Wrapped>>,
    name: String,
}

#[test]
fn test_option_map_with_present() {
    let toml_str = r#"
name = "test"
[entries]
a = "alpha"
b = "beta"
"#;
    let parsed =
        WithOptionMap::tpd_from_toml(toml_str, FieldMatchMode::Exact, VecMode::Strict).unwrap();
    assert_eq!(parsed.name, "test");
    let entries = parsed.entries.expect("entries should be Some");
    assert_eq!(entries.len(), 2);
    assert_eq!(entries["a"].0, "alpha");
    assert_eq!(entries["b"].0, "beta");
}

#[test]
fn test_option_map_with_absent() {
    let toml_str = r#"name = "test""#;
    let parsed =
        WithOptionMap::tpd_from_toml(toml_str, FieldMatchMode::Exact, VecMode::Strict).unwrap();
    assert_eq!(parsed.name, "test");
    assert!(parsed.entries.is_none());
}
