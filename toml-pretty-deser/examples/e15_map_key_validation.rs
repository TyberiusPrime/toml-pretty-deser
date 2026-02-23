//! # Example 15: Map key validation with `verify_keys`
//!
//! Demonstrates `FailableKeys::verify_keys` for validating the keys of an
//! `IndexMap` field. The macro generates a `MapAndKeys<K, V>` in the partial
//! struct which exposes `verify_keys`, letting you report per-key errors with
//! proper source-location highlighting.

use indexmap::IndexMap;
use toml_pretty_deser::prelude::*;

#[tpd(root)]
#[derive(Debug)]
struct ServiceConfig {
    name: String,
    /// Label keys must be lowercase identifiers: [a-z][a-z0-9_-]*
    labels: IndexMap<String, String>,
}

impl VerifyIn<TPDRoot> for PartialServiceConfig {
    fn verify(&mut self, _parent: &TPDRoot) -> Result<(), ValidationFailure> {
        self.labels.verify_keys(|key| {
            let first = key.chars().next().unwrap_or('_');
            if !first.is_ascii_lowercase() {
                return Err(ValidationFailure::new(
                    "Label key must start with a lowercase letter",
                    Some("Use [a-z][a-z0-9_-]* format, e.g. 'my-label'"),
                ));
            }
            if !key
                .chars()
                .all(|c| c.is_ascii_lowercase() || c.is_ascii_digit() || c == '_' || c == '-')
            {
                return Err(ValidationFailure::new(
                    "Label key contains invalid characters",
                    Some("Only lowercase letters, digits, hyphens, and underscores are allowed"),
                ));
            }
            Ok(())
        });
        Ok(())
    }
}

fn main() {
    // Valid config
    let toml_ok = r#"
name = "my-service"

[labels]
environment = "production"
team = "platform"
version = "1.0"
"#;

    println!("--- Valid labels ---");
    match ServiceConfig::tpd_from_toml(toml_ok, FieldMatchMode::Exact, VecMode::Strict) {
        Ok(config) => {
            println!("Service: {}", config.name);
            println!("Labels:");
            for (k, v) in &config.labels {
                println!("  {k} = {v}");
            }
        }
        Err(e) => eprintln!("{}", e.pretty("service.toml")),
    }

    // Invalid: bad key names
    let toml_bad = r#"
name = "my-service"

[labels]
valid-key = "ok"
Invalid_Key = "bad - starts with uppercase"
123starts = "bad - starts with digit"
"#;

    println!("\n--- Invalid label keys ---");
    match ServiceConfig::tpd_from_toml(toml_bad, FieldMatchMode::Exact, VecMode::Strict) {
        Ok(_) => unreachable!(),
        Err(e) => eprintln!("{}", e.pretty("service.toml")),
    }
}
