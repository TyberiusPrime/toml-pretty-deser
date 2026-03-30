//! Two different variants sharing the same alias must be a compile error.
use toml_pretty_deser::prelude::*;

#[tpd]
#[derive(Debug)]
enum Color {
    #[tpd(alias = "primary")]
    Red,
    #[tpd(alias = "primary")]
    Blue,
}

fn main() {}
