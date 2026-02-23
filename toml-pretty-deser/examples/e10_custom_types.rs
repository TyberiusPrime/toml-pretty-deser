//! # Example 10: Custom types with `impl_visitor!` and `impl_visitor_for_try_from_str!`
//!
//! Shows how to make custom types work with toml-pretty-deser:
//! - `impl_visitor_for_from_str!` for types implementing `From<&str>`
//! - `impl_visitor_for_try_from_str!` for types implementing `TryFrom<&str>`
//! - `impl_visitor!` for full control over deserialization

use toml_pretty_deser::prelude::*;

// --- From<&str> example ---

#[derive(Debug)]
struct Email(String);

impl From<&str> for Email {
    fn from(s: &str) -> Self {
        Email(s.to_string())
    }
}

impl_visitor_for_from_str!(Email);

// --- TryFrom<&str> example ---

#[derive(Debug)]
struct Port(u16);

impl TryFrom<&str> for Port {
    type Error = String;
    fn try_from(s: &str) -> Result<Self, String> {
        let n: u16 = s.parse().map_err(|e| format!("{e}"))?;
        if n > 0 {
            Ok(Port(n))
        } else {
            Err("port must be > 0".to_string())
        }
    }
}

impl_visitor_for_try_from_str!(Port, "Invalid port");

// --- Full impl_visitor! example ---

#[derive(Debug)]
struct HexColor(u8, u8, u8);

impl_visitor!(HexColor, |helper| {
    match helper.item.as_str() {
        Some(s) if s.starts_with('#') && s.len() == 7 => {
            let r = u8::from_str_radix(&s[1..3], 16);
            let g = u8::from_str_radix(&s[3..5], 16);
            let b = u8::from_str_radix(&s[5..7], 16);
            match (r, g, b) {
                (Ok(r), Ok(g), Ok(b)) => TomlValue::new_ok(HexColor(r, g, b), helper.span()),
                _ => TomlValue::new_validation_failed(
                    helper.span(),
                    "Invalid hex digits".to_string(),
                    Some("Use format '#RRGGBB' with valid hex digits".to_string()),
                ),
            }
        }
        Some(_) => TomlValue::new_validation_failed(
            helper.span(),
            "Invalid color format".to_string(),
            Some("Use format '#RRGGBB', e.g. '#ff0000'".to_string()),
        ),
        None => TomlValue::new_wrong_type(helper.item, helper.span(), "string"),
    }
});

#[tpd(root, no_verify)]
#[derive(Debug)]
struct ThemeConfig {
    admin_email: Email,
    api_port: Port,
    background: HexColor,
}

fn main() {
    let toml_str = r##"
admin_email = "admin@example.com"
api_port = "8080"
background = "#1a2b3c"
"##;

    match ThemeConfig::tpd_from_toml(toml_str, FieldMatchMode::Exact, VecMode::Strict) {
        Ok(config) => {
            println!("Email: {}", config.admin_email.0);
            println!("Port:  {}", config.api_port.0);
            println!(
                "Color: rgb({}, {}, {})",
                config.background.0, config.background.1, config.background.2
            );
        }
        Err(e) => eprintln!("{}", e.pretty("theme.toml")),
    }

    // Error case: invalid port and color
    let bad_toml = r##"
admin_email = "test@test.com"
api_port = "not-a-number"
background = "#xyz"
"##;

    println!("\n--- Validation errors ---");
    match ThemeConfig::tpd_from_toml(bad_toml, FieldMatchMode::Exact, VecMode::Strict) {
        Ok(_) => unreachable!(),
        Err(e) => eprintln!("{}", e.pretty("theme.toml")),
    }
}
