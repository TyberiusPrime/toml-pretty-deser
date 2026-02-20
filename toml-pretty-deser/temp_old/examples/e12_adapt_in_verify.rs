//! # Example 12: Two-phase transformation with `adapt_in_verify`
//!
//! Shows `#[tpd(adapt_in_verify)]` and `#[tpd(adapt_in_verify(Type))]` for
//! transforming fields during the verification phase. This is useful when a
//! field's TOML type differs from its final Rust type.
//!
//! Also demonstrates the nested adapt pattern with `Rc<RefCell<T>>`.

use std::cell::RefCell;
use std::rc::Rc;
use toml_pretty_deser::prelude::*;

// --- Leaf adapt_in_verify ---

#[tpd(root)]
#[derive(Debug)]
struct Config {
    /// Deserializes as a toml_edit::Item (the default for bare adapt_in_verify),
    /// then converted to usize (string length) in verify.
    #[tpd(adapt_in_verify)]
    name_length: usize,

    /// Deserializes as String first, then parsed to usize in verify.
    #[tpd(adapt_in_verify(String))]
    count: usize,
}

impl VerifyIn<TPDRoot> for PartialConfig {
    fn verify(&mut self, _parent: &TPDRoot) -> Result<(), ValidationFailure> {
        // adapt_in_verify (bare): receives toml_edit::Item
        self.name_length.adapt(|item, span| {
            match item.as_str() {
                Some(s) => TomlValue::new_ok(s.len(), span),
                None => TomlValue::new_wrong_type(&item, span, "string"),
            }
        });

        // adapt_in_verify(String): receives String
        self.count.adapt(|s, span| {
            match s.parse::<usize>() {
                Ok(n) => TomlValue::new_ok(n, span),
                Err(_) => TomlValue::new_validation_failed(
                    span,
                    "Not a valid number".to_string(),
                    Some("Provide a numeric string like '42'".to_string()),
                ),
            }
        });

        Ok(())
    }
}

// --- Nested adapt_in_verify with Rc<RefCell<T>> ---

#[tpd(no_verify)]
#[derive(Debug)]
struct InnerData {
    value: u32,
    label: String,
}

#[tpd(root)]
#[derive(Debug)]
struct SharedConfig {
    name: String,
    #[tpd(adapt_in_verify, nested)]
    shared_data: Rc<RefCell<InnerData>>,
}

impl VerifyIn<TPDRoot> for PartialSharedConfig {
    fn verify(&mut self, _parent: &TPDRoot) -> Result<(), ValidationFailure> {
        // Nested adapt: receives the concrete InnerData (already converted from partial)
        self.shared_data.adapt(|inner, span| {
            TomlValue::new_ok(Rc::new(RefCell::new(inner)), span)
        });
        Ok(())
    }
}

fn main() {
    // Leaf adapt examples
    let toml_str = r#"
name_length = "hello"
count = "42"
"#;

    println!("--- Leaf adapt_in_verify ---");
    match Config::tpd_from_toml(toml_str, FieldMatchMode::Exact, VecMode::Strict) {
        Ok(config) => {
            println!("name_length: {} (length of 'hello')", config.name_length);
            println!("count: {} (parsed from '42')", config.count);
        }
        Err(e) => eprintln!("{}", e.pretty("config.toml")),
    }

    // Nested adapt example
    let toml_nested = r#"
name = "shared-app"
[shared_data]
value = 100
label = "important"
"#;

    println!("\n--- Nested adapt_in_verify (Rc<RefCell<T>>) ---");
    match SharedConfig::tpd_from_toml(toml_nested, FieldMatchMode::Exact, VecMode::Strict) {
        Ok(config) => {
            let data = config.shared_data.borrow();
            println!("name: {}", config.name);
            println!("shared_data: value={}, label={}", data.value, data.label);
        }
        Err(e) => eprintln!("{}", e.pretty("config.toml")),
    }
}
