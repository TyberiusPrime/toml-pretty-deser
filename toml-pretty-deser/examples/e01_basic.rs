//! # Example 01: Basic struct with no custom validation
//!
//! Demonstrates the simplest usage of `toml-pretty-deser`: a root struct with
//! primitive fields (String, i64, bool, f64) and `no_verify` to skip custom validation.

use toml_pretty_deser::prelude::*;

#[tpd(root, no_verify)]
#[derive(Debug)]
struct ServerConfig {
    host: String,
    port: i64,
    debug: bool,
    timeout: f64,
}

fn main() {
    let toml_str = r#"
host = "localhost"
port = 8080
debug = true
timeout = 30.5
"#;

    match ServerConfig::tpd_from_toml(toml_str, FieldMatchMode::Exact, VecMode::Strict) {
        Ok(config) => {
            println!("Parsed config successfully!");
            println!("  host:    {}", config.host);
            println!("  port:    {}", config.port);
            println!("  debug:   {}", config.debug);
            println!("  timeout: {}", config.timeout);
        }
        Err(e) => {
            eprintln!("{}", e.pretty("config.toml"));
        }
    }
}
