//! # Example 05: Custom verification with VerifyIn
//!
//! Shows how to implement `VerifyIn<Root>` on the generated `PartialT` struct
//! to add custom validation logic. Demonstrates:
//! - `self.field.verify(|v| ...)` for single-field validation
//! - `TomlValue::new_custom(...)` for multi-span errors

#![allow(dead_code)]
use toml_pretty_deser::prelude::*;

#[tpd(root)]
#[derive(Debug)]
struct ServerConfig {
    host: String,
    port: u16,
    max_connections: u32,
    min_connections: u32,
}

impl VerifyIn<TPDRoot> for PartialServerConfig {
    fn verify(&mut self, _parent: &TPDRoot) -> Result<(), ValidationFailure> {
        // Single-field validation: port must be > 1024
        self.port.verify(|p| {
            if *p > 1024 {
                Ok(())
            } else {
                Err(ValidationFailure::new(
                    "Port must be greater than 1024",
                    Some("Use a port in the range 1025..65535"),
                ))
            }
        });

        // Multi-span error: max_connections must be >= min_connections
        if let Some(max) = self.max_connections.as_ref()
            && let Some(min) = self.min_connections.as_ref()
            && max < min
        {
            let max_val = *max;
            // Create a custom error pointing at both fields
            let spans = vec![
                (
                    self.max_connections.span(),
                    "max_connections is here".to_string(),
                ),
                (
                    self.min_connections.span(),
                    "min_connections is here".to_string(),
                ),
            ];
            // Store the error on a field — using new_custom on max_connections
            self.max_connections = TomlValue::new_custom(
                Some(max_val),
                spans,
                Some("max_connections must be >= min_connections"),
            );
        }

        Ok(())
    }
}

fn main() {
    // Valid config
    let toml_ok = r#"
host = "localhost"
port = 8080
max_connections = 100
min_connections = 10
"#;

    match ServerConfig::tpd_from_toml(toml_ok, FieldMatchMode::Exact, VecMode::Strict) {
        Ok(config) => println!("Valid config: {:?}", config),
        Err(e) => eprintln!("{}", e.pretty("server.toml")),
    }

    // Invalid: port too low + max < min
    let toml_bad = r#"
host = "localhost"
port = 80
max_connections = 5
min_connections = 50
"#;

    println!("\n--- Validation errors ---");
    match ServerConfig::tpd_from_toml(toml_bad, FieldMatchMode::Exact, VecMode::Strict) {
        Ok(_) => unreachable!(),
        Err(e) => eprintln!("{}", e.pretty("server.toml")),
    }
}
