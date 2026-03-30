//! An alias on one variant that matches another variant's canonical name must be a compile error.
use toml_pretty_deser::prelude::*;

#[tpd]
#[derive(Debug)]
enum Color {
    #[tpd(alias = "Blue")]
    Red,
    Blue,
}

fn main() {}
