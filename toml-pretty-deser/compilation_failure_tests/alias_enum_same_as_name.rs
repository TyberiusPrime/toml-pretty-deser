//! Alias equal to the variant's canonical name must be a compile error.
use toml_pretty_deser::prelude::*;

#[tpd]
#[derive(Debug)]
enum Color {
    #[tpd(alias = "Red")]
    Red,
    Blue,
}

fn main() {}
