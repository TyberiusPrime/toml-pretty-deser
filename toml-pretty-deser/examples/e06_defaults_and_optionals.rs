//! # Example 06: Defaults, optionals, and skip
//!
//! Demonstrates `#[tpd(default)]`, `Option<T>` fields, `#[tpd(skip)]`,
//! and manual `.or()` / `.or_with()` / `.or_default()` in a VerifyIn impl.

use toml_pretty_deser::prelude::*;

#[tpd(root)]
#[derive(Debug)]
struct AppConfig {
    name: String,

    /// Optional field: becomes None if missing from TOML
    description: Option<String>,

    /// Default attribute: uses Default::default() (0 for u32) if missing
    #[tpd(default)]
    retries: u32,

    /// We'll set a manual default via .or_with() in verify
    timeout_ms: u64,

    /// We'll set a manual default via .or() in verify
    log_level: String,

    /// Skipped field: not read from TOML, set programmatically
    #[tpd(skip)]
    computed_id: String,
}

impl VerifyIn<Root> for PartialAppConfig {
    fn verify(&mut self, _parent: &Root) -> Result<(), ValidationFailure> {
        // .or_with(): provide a default via closure if missing
        self.timeout_ms.or_with(|| 5000);

        // .or(): provide a literal default if missing
        self.log_level.or("info".to_string());

        // Skip field: set it directly (it's Option<T> in the partial)
        self.computed_id = Some("auto-generated-id".to_string());

        Ok(())
    }
}

fn main() {
    // Minimal config: most fields use defaults
    let toml_minimal = r#"
name = "my-app"
"#;

    match AppConfig::tpd_from_toml(toml_minimal, FieldMatchMode::Exact, VecMode::Strict) {
        Ok(config) => {
            println!("Minimal config:");
            println!("  name:        {}", config.name);
            println!("  description: {:?}", config.description);
            println!("  retries:     {}", config.retries);
            println!("  timeout_ms:  {}", config.timeout_ms);
            println!("  log_level:   {}", config.log_level);
            println!("  computed_id: {}", config.computed_id);
        }
        Err(e) => eprintln!("{}", e.pretty("config.toml")),
    }

    // Full config: all fields provided
    let toml_full = r#"
name = "my-app"
description = "A great application"
retries = 3
timeout_ms = 10000
log_level = "debug"
"#;

    println!("\nFull config:");
    match AppConfig::tpd_from_toml(toml_full, FieldMatchMode::Exact, VecMode::Strict) {
        Ok(config) => {
            println!("  name:        {}", config.name);
            println!("  description: {:?}", config.description);
            println!("  retries:     {}", config.retries);
            println!("  timeout_ms:  {}", config.timeout_ms);
            println!("  log_level:   {}", config.log_level);
            println!("  computed_id: {}", config.computed_id);
        }
        Err(e) => eprintln!("{}", e.pretty("config.toml")),
    }
}
