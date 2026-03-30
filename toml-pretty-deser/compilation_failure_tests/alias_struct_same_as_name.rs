//! Alias equal to the field's canonical name must be a compile error.
use toml_pretty_deser::prelude::*;

#[tpd]
#[derive(Debug)]
struct Foo {
    #[tpd(alias = "bar")]
    bar: String,
}

fn main() {}
