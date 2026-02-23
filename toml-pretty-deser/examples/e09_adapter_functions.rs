//! # Example 09: Adapter functions with `#[tpd(with = "fn")]`
//!
//! Shows how to use adapter functions that transform values during
//! deserialization. The adapter function signature is:
//! `fn(TomlValue<InputType>) -> TomlValue<OutputType>`
//! (which means that both types must implement `Visitor`

use toml_pretty_deser::prelude::*;

/// Adapter: read a string from TOML and uppercase it.
fn to_uppercase(input: TomlValue<String>) -> TomlValue<String> {
    input.map(|s| s.to_uppercase())
}

/// Adapter: read an integer from TOML and convert it to a String.
fn int_to_string(input: TomlValue<i64>) -> TomlValue<String> {
    input.map(|n| n.to_string())
}

#[tpd(root, no_verify)]
#[derive(Debug)]
struct Config {
    #[tpd(with = "to_uppercase")]
    environment: String,

    #[tpd(with = "int_to_string")]
    version_string: String,

    /// Regular field for comparison
    name: String,
}

fn main() {
    let toml_str = r#"
environment = "production"
version_string = 42
name = "my-service"
"#;

    match Config::tpd_from_toml(toml_str, FieldMatchMode::Exact, VecMode::Strict) {
        Ok(config) => {
            println!(
                "environment:    {} (was 'production', uppercased by adapter)",
                config.environment
            );
            println!(
                "version_string: {} (was integer 42, converted to string)",
                config.version_string
            );
            println!(
                "name:           {} (no adapter, passed through)",
                config.name
            );
        }
        Err(e) => eprintln!("{}", e.pretty("config.toml")),
    }
}
