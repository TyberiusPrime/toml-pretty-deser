// Tests: IndexMap with a key type backed by TryFrom<String>.
//
// When TryFrom<String> fails for a key, the library must record a
// ValidationFailed error on that key's TomlValue and propagate the error up,
// so tpd_from_toml returns Err and the pretty output contains the TryFrom
// error message.

use indexmap::IndexMap;
use toml_pretty_deser::prelude::*;

/// A port number key. Only accepts decimal strings in 1..=65535.
#[derive(Debug, PartialEq, Eq, Hash)]
struct Port(u16);

impl TryFrom<String> for Port {
    type Error = String;

    fn try_from(s: String) -> Result<Self, Self::Error> {
        s.parse::<u16>()
            .map_err(|e| format!("not a valid port number: {e}"))
            .and_then(|n| {
                if n == 0 {
                    Err("port 0 is reserved".to_string())
                } else {
                    Ok(Port(n))
                }
            })
    }
}

#[tpd(root, no_verify)]
#[derive(Debug)]
struct PortConfig {
    services: IndexMap<Port, String>,
}

// --- success cases ---

#[test]
fn valid_port_keys_parse_ok() {
    let toml = r#"
[services]
8080 = "http"
443 = "https"
22 = "ssh"
"#;
    let config =
        PortConfig::tpd_from_toml(toml, FieldMatchMode::Exact, VecMode::Strict).unwrap();
    assert_eq!(config.services.len(), 3);
    assert_eq!(config.services[&Port(8080)], "http");
    assert_eq!(config.services[&Port(443)], "https");
    assert_eq!(config.services[&Port(22)], "ssh");
}

// --- failure cases ---

#[test]
fn non_numeric_port_key_gives_validation_error() {
    let toml = r#"
[services]
8080 = "http"
not-a-port = "bad"
"#;
    let result = PortConfig::tpd_from_toml(toml, FieldMatchMode::Exact, VecMode::Strict);
    assert!(result.is_err(), "non-numeric key should fail");
    let pretty = result.unwrap_err().pretty("test.toml");
    assert!(
        pretty.contains("not a valid port number"),
        "expected TryFrom error in output, got:\n{pretty}"
    );
    insta::assert_snapshot!(pretty);
}

#[test]
fn reserved_port_zero_gives_validation_error() {
    let toml = r#"
[services]
0 = "reserved"
"#;
    let result = PortConfig::tpd_from_toml(toml, FieldMatchMode::Exact, VecMode::Strict);
    assert!(result.is_err(), "port 0 should fail");
    let pretty = result.unwrap_err().pretty("test.toml");
    assert!(
        pretty.contains("port 0 is reserved"),
        "expected reserved-port error in output, got:\n{pretty}"
    );
    insta::assert_snapshot!(pretty);
}

#[test]
fn mixed_valid_and_invalid_keys_both_reported() {
    let toml = r#"
[services]
8080 = "http"
bad-key = "bad"
443 = "https"
also-bad = "also bad"
"#;
    let result = PortConfig::tpd_from_toml(toml, FieldMatchMode::Exact, VecMode::Strict);
    assert!(result.is_err(), "any invalid key should fail");
    let pretty = result.unwrap_err().pretty("test.toml");
    // Both bad keys should appear
    assert_eq!(
        pretty.matches("not a valid port number").count(),
        2,
        "both invalid keys should be reported, got:\n{pretty}"
    );
    insta::assert_snapshot!(pretty);
}
