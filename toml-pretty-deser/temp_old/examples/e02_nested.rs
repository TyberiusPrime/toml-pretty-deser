//! # Example 02: Nested structs
//!
//! Shows how to nest one struct inside another using `#[tpd(nested)]`.
//! The inner struct also uses `#[tpd]` (or `#[tpd(no_verify)]`), and the
//! outer struct uses `#[tpd(root)]`.

use toml_pretty_deser::prelude::*;

#[tpd(no_verify)]
#[derive(Debug)]
struct DatabaseConfig {
    host: String,
    port: u16,
    name: String,
}

#[tpd(root, no_verify)]
#[derive(Debug)]
struct AppConfig {
    app_name: String,
    #[tpd(nested)]
    database: DatabaseConfig,
}

fn main() {
    let toml_str = r#"
app_name = "my-service"

[database]
host = "db.example.com"
port = 5432
name = "mydb"
"#;

    match AppConfig::tpd_from_toml(toml_str, FieldMatchMode::Exact, VecMode::Strict) {
        Ok(config) => {
            println!("App: {}", config.app_name);
            println!("DB:  {}:{}/{}", config.database.host, config.database.port, config.database.name);
        }
        Err(e) => {
            eprintln!("{}", e.pretty("config.toml"));
        }
    }
}
