//! The same alias listed twice on the same variant must be a compile error.
use toml_pretty_deser::prelude::*;

#[tpd]
#[derive(Debug)]
enum Color {
    #[tpd(alias = "r", alias = "r")]
    Red,
    Blue,
}

fn main() {}
