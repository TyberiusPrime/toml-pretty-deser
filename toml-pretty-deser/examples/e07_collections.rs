//! # Example 07: Vec and IndexMap collections
//!
//! Demonstrates `Vec<T>`, `Vec<NestedStruct>`, `IndexMap<String, T>`, and
//! `IndexMap<String, NestedStruct>`. Shows `[[array_of_tables]]` TOML syntax
//! for arrays of nested structs.

use indexmap::IndexMap;
use toml_pretty_deser::prelude::*;

#[tpd(no_verify)]
#[derive(Debug)]
struct Endpoint {
    path: String,
    method: String,
}

#[tpd(root, no_verify)]
#[derive(Debug)]
struct ApiConfig {
    /// Simple vec of strings
    allowed_origins: Vec<String>,

    /// Vec of nested structs — uses [[endpoints]] in TOML
    #[tpd(nested)]
    endpoints: Vec<Endpoint>,

    /// IndexMap of simple values — uses [headers] table in TOML
    headers: IndexMap<String, String>,

    /// IndexMap of nested structs — uses [routes.name] tables in TOML
    #[tpd(nested)]
    routes: IndexMap<String, Endpoint>,
}

fn main() {
    let toml_str = r#"
allowed_origins = ["https://example.com", "https://app.example.com"]

[headers]
X-Api-Version = "2.0"
X-Request-Id = "auto"

[[endpoints]]
path = "/health"
method = "GET"

[[endpoints]]
path = "/users"
method = "POST"

[routes.dashboard]
path = "/dashboard"
method = "GET"

[routes.settings]
path = "/settings"
method = "PUT"
"#;

    match ApiConfig::tpd_from_toml(toml_str, FieldMatchMode::Exact, VecMode::Strict) {
        Ok(config) => {
            println!("Origins: {:?}", config.allowed_origins);
            println!("\nEndpoints:");
            for ep in &config.endpoints {
                println!("  {} {}", ep.method, ep.path);
            }
            println!("\nHeaders:");
            for (k, v) in &config.headers {
                println!("  {k}: {v}");
            }
            println!("\nRoutes:");
            for (name, ep) in &config.routes {
                println!("  {name}: {} {}", ep.method, ep.path);
            }
        }
        Err(e) => eprintln!("{}", e.pretty("api.toml")),
    }
}
