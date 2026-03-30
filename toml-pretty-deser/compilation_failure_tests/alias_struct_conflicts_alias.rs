//! Two different fields sharing the same alias must be a compile error.
use toml_pretty_deser::prelude::*;

#[tpd]
#[derive(Debug)]
struct Foo {
    #[tpd(alias = "x")]
    a: u32,
    #[tpd(alias = "x")]
    b: u32,
}

fn main() {}
