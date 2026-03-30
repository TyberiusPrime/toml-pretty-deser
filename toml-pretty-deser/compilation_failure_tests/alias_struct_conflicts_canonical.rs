//! An alias on one field that matches another field's canonical name must be a compile error.
use toml_pretty_deser::prelude::*;

#[tpd]
#[derive(Debug)]
struct Foo {
    name: String,
    #[tpd(alias = "name")]
    value: u32,
}

fn main() {}
