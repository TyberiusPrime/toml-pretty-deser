//! # Example 11: Absorb remaining keys with `#[tpd(absorb_remaining)]`
//!
//! Shows how to capture all unmatched TOML keys into an `IndexMap`,
//! useful for extension fields or pass-through configuration.

use indexmap::IndexMap;
use toml_pretty_deser::prelude::*;

#[tpd(root, no_verify)]
#[derive(Debug)]
struct PluginConfig {
    /// Known field
    name: String,
    /// Known field
    enabled: bool,
    /// All other keys are captured here
    #[tpd(absorb_remaining)]
    settings: IndexMap<String, String>,
}

fn main() {
    let toml_str = r#"
name = "image-resizer"
enabled = true
max_width = "1920"
max_height = "1080"
format = "webp"
quality = "85"
"#;

    match PluginConfig::tpd_from_toml(toml_str, FieldMatchMode::Exact, VecMode::Strict) {
        Ok(config) => {
            println!("Plugin: {} (enabled={})", config.name, config.enabled);
            println!("Extra settings:");
            for (k, v) in &config.settings {
                println!("  {k} = {v}");
            }
        }
        Err(e) => eprintln!("{}", e.pretty("plugin.toml")),
    }
}
