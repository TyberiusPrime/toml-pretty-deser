//! Test that #[tpd_absorb_remaining] on HashMap is a compile error (must be IndexMap)
use std::collections::HashMap;
use toml_pretty_deser::prelude::*;

#[tpd(root)]
#[derive(Debug)]
struct WrongTypeHashMap {
    name: String,
    #[tpd(absorb_remaining)]
    extra: HashMap<String, String>,
}

fn main() {}
