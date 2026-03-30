//! The same alias listed twice on the same field must be a compile error.
use toml_pretty_deser::prelude::*;

#[tpd]
#[derive(Debug)]
struct Foo {
    name: String,
    #[tpd(alias = "x", alias = "x")]
    value: u32,
}

fn main() {}
