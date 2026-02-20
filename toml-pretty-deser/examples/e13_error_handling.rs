//! # Example 13: Working with DeserError and partial results
//!
//! Shows how to handle `DeserError::DeserFailure` to access both error messages
//! and partial results. This is the library's key differentiator: even when
//! deserialization fails, you get back the partial struct with whatever fields
//! were successfully parsed.

#![allow(dead_code)]
use toml_pretty_deser::prelude::*;

#[tpd(root, no_verify)]
#[derive(Debug)]
struct Config {
    name: String,
    port: u16,
    debug: bool,
    workers: u8,
}

fn main() {
    // TOML with multiple errors: port is wrong type, workers overflows u8
    let toml_str = r#"
name = "my-app"
port = "not-a-number"
debug = true
workers = 999
"#;

    match Config::tpd_from_toml(toml_str, FieldMatchMode::Exact, VecMode::Strict) {
        Ok(config) => {
            println!("Config: {:?}", config);
        }
        Err(ref e @ DeserError::DeserFailure(ref errors, ref partial)) => {
            // Pretty-print the errors
            println!("=== Pretty error output ===");
            println!("{}", e.pretty("config.toml"));

            // Access individual errors
            println!("=== {} error(s) found ===", errors.len());
            for (i, err) in errors.iter().enumerate() {
                println!("Error {}: {}", i + 1, err.pretty("config.toml"));
            }

            // Access the partial result: successfully-parsed fields are available!
            println!("=== Partial result ===");
            println!("  name (ok):    {:?}", partial.name.as_ref());
            println!("  port (err):   {:?}", partial.port.as_ref());
            println!("  debug (ok):   {:?}", partial.debug.as_ref());
            println!("  workers (err):{:?}", partial.workers.as_ref());
        }
        Err(DeserError::ParsingFailure(toml_err, _source)) => {
            // TOML syntax error — no partial available
            eprintln!("TOML syntax error: {toml_err}");
        }
    }
}
