//! # Example 03: Simple string enums
//!
//! Demonstrates `#[tpd]` on a unit-variant enum, which deserializes from a
//! TOML string value matching the variant name.

use toml_pretty_deser::prelude::*;

#[tpd]
#[derive(Debug)]
enum LogLevel {
    Debug,
    Info,
    Warning,
    Error,
}

#[tpd(root, no_verify)]
#[derive(Debug)]
struct Config {
    log_level: LogLevel,
    name: String,
}

fn main() {
    // Happy path
    let toml_str = r#"
log_level = "Info"
name = "my-app"
"#;

    match Config::tpd_from_toml(toml_str, FieldMatchMode::Exact, VecMode::Strict) {
        Ok(config) => println!("Log level: {:?}, name: {}", config.log_level, config.name),
        Err(e) => eprintln!("{}", e.pretty("config.toml")),
    }

    // Error path: invalid variant
    let bad_toml = r#"
log_level = "Verbose"
name = "my-app"
"#;

    println!("\n--- Invalid enum value ---");
    match Config::tpd_from_toml(bad_toml, FieldMatchMode::Exact, VecMode::Strict) {
        Ok(_) => unreachable!(),
        Err(e) => eprintln!("{}", e.pretty("config.toml")),
    }
}
