//! Test that having multiple #[tpd_absorb_remaining] fields is a compile error
use indexmap::IndexMap;
use toml_pretty_deser::prelude::*;

#[tpd]
#[derive(Debug)]
struct MultipleAbsorb {
    name: String,
    #[tpd(absorb_remaining)]
    extra1: IndexMap<String, String>,
    #[tpd(absorb_remaining)]
    extra2: IndexMap<String, String>,
}

fn main() {}
