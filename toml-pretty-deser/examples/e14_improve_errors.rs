//! # Example 14: Improving error messages downstream
//!
//! Shows how to iterate over partial results in `VerifyIn` and amend the help
//! text on errors — for example, adding links to per-plugin documentation.
//!
//! The key idea: inside `VerifyIn::verify` you have mutable access to every
//! `TomlValue` field. Since `TomlValue.help` lives on the struct (not per-variant),
//! you can set help text on any error state without changing the error kind.

#![allow(dead_code)]
use toml_pretty_deser::prelude::*;

#[tpd(no_verify)]
#[derive(Debug)]
struct Plugin {
    name: String,
    version: u32,
    enabled: bool,
}

#[tpd(root)]
#[derive(Debug)]
struct Config {
    #[tpd(nested)]
    plugins: Vec<Plugin>,
}

impl VerifyIn<TPDRoot> for PartialConfig {
    fn verify(&mut self, _parent: &TPDRoot) -> Result<(), ValidationFailure> {
        // Iterate through all plugins in the partial vec and amend errors
        if let Some(plugins_vec) = &mut self.plugins.value {
            for (i, plugin_tv) in plugins_vec.iter_mut().enumerate() {
                if let Some(plugin) = &mut plugin_tv.value {
                    // Build a doc link specific to this plugin's index/name
                    let plugin_name = plugin.name.as_ref().map_or("unknown", |s| s.as_str());
                    let doc_link = format!(
                        "See https://docs.example.com/plugins/{plugin_name} for the schema"
                    );

                    amend_help(&mut plugin.name, &doc_link);
                    amend_help(&mut plugin.version, &doc_link);
                    amend_help(&mut plugin.enabled, &doc_link);
                }

                // Also handle the case where the plugin table itself had
                // unknown keys (the error lives on plugin_tv, not the inner fields)
                if let TomlValueState::UnknownKeys(keys) = &mut plugin_tv.state {
                    let doc_link = format!(
                        "See https://docs.example.com/plugins for allowed keys (plugin #{})",
                        i
                    );
                    for key in keys {
                        key.help = format!("{}\n{doc_link}", key.help);
                    }
                }
            }
        }
        Ok(())
    }
}

/// Append a documentation link to the help text of a single TomlValue,
/// if it is in an error state.
fn amend_help<T>(field: &mut TomlValue<T>, extra: &str) {
    if field.is_ok() {
        return;
    }
    let combined = match field.help.take() {
        Some(existing) => format!("{existing}\n{extra}"),
        None => extra.to_string(),
    };
    field.help = Some(combined);
}

fn main() {
    // TOML with several errors across plugins:
    // - plugin 0: "version" has wrong type (string instead of integer)
    // - plugin 1: "enabled" is missing
    // - plugin 2: has an unknown key "colour"
    let toml_str = r#"
[[plugins]]
name = "image-resize"
version = "latest"
enabled = true

[[plugins]]
name = "cache-buster"
version = 2

[[plugins]]
name = "theme-engine"
version = 1
enabled = false
colour = "blue"
"#;

    match Config::tpd_from_toml(toml_str, FieldMatchMode::Exact, VecMode::Strict) {
        Ok(config) => println!("All good: {:?}", config),
        Err(e) => {
            println!("Errors with amended help text:\n");
            println!("{}", e.pretty("plugins.toml"));
        }
    }
    panic!();
}
