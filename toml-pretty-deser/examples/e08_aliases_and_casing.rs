//! # Example 08: Field aliases, enum variant aliases, and FieldMatchMode
//!
//! Shows `#[tpd(alias = "...")]` on fields and enum variants,
//! `FieldMatchMode::AnyCase` for case-insensitive matching, and
//! `VecMode::SingleOk` to allow single values where arrays are expected.


#![allow(dead_code)]
use toml_pretty_deser::prelude::*;

/// Simple enum with variant aliases.
/// "Warn" and "Caution" both map to the Warning variant.
#[tpd]
#[derive(Debug)]
enum Severity {
    Info,
    #[tpd(alias = "Warn", alias = "Caution")]
    Warning,
    Error,
    #[tpd(alias = "Fatal")]
    Critical,
}

#[tpd(root, no_verify)]
#[derive(Debug)]
struct Config {
    /// Accepts "host", "hostname", or "server" in TOML
    #[tpd(alias = "hostname", alias = "server")]
    host: String,

    /// Accepts "port" or "listen_port"
    #[tpd(alias = "listen_port")]
    port: u16,

    /// Vec field: with SingleOk, `tags = "web"` becomes `vec!["web"]`
    tags: Vec<String>,

    /// Enum field with variant aliases
    severity: Severity,
}

fn main() {
    // Using field aliases + canonical enum variant name
    let toml_aliases = r#"
hostname = "example.com"
listen_port = 443
tags = ["web", "production"]
severity = "Warning"
"#;

    println!("--- Field aliases + canonical enum variant ---");
    match Config::tpd_from_toml(toml_aliases, FieldMatchMode::Exact, VecMode::Strict) {
        Ok(config) => println!("{:?}", config),
        Err(e) => eprintln!("{}", e.pretty("config.toml")),
    }

    // Using enum variant aliases: "Warn" and "Fatal" resolve to Warning and Critical
    let toml_enum_aliases = r#"
host = "example.com"
port = 8080
tags = ["api"]
severity = "Warn"
"#;

    println!("\n--- Enum variant alias 'Warn' -> Warning ---");
    match Config::tpd_from_toml(toml_enum_aliases, FieldMatchMode::Exact, VecMode::Strict) {
        Ok(config) => println!("{:?}", config),
        Err(e) => eprintln!("{}", e.pretty("config.toml")),
    }

    let toml_fatal = r#"
host = "example.com"
port = 9090
tags = ["monitoring"]
severity = "Fatal"
"#;

    println!("\n--- Enum variant alias 'Fatal' -> Critical ---");
    match Config::tpd_from_toml(toml_fatal, FieldMatchMode::Exact, VecMode::Strict) {
        Ok(config) => println!("{:?}", config),
        Err(e) => eprintln!("{}", e.pretty("config.toml")),
    }

    // AnyCase mode: field names are case-insensitive
    let toml_anycase = r#"
HOST = "example.com"
Port = 8080
TAGS = ["api"]
SEVERITY = "Caution"
"#;

    println!("\n--- AnyCase mode + enum alias 'Caution' -> Warning ---");
    match Config::tpd_from_toml(toml_anycase, FieldMatchMode::AnyCase, VecMode::Strict) {
        Ok(config) => println!("{:?}", config),
        Err(e) => eprintln!("{}", e.pretty("config.toml")),
    }

    // SingleOk vec mode: single values auto-wrap into arrays
    let toml_single = r#"
host = "localhost"
port = 3000
tags = "dev"
severity = "Info"
"#;

    println!("\n--- SingleOk vec mode ---");
    match Config::tpd_from_toml(toml_single, FieldMatchMode::Exact, VecMode::SingleOk) {
        Ok(config) => println!("{:?}", config),
        Err(e) => eprintln!("{}", e.pretty("config.toml")),
    }
}
