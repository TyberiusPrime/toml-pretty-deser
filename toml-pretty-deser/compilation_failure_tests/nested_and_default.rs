use toml_pretty_deser::prelude::*;

#[tpd(root)]
#[derive(Debug)]
struct NestedAndDefault {
    #[tpd(nested, default)]
    nested: Inner,
}

#[tpd]
#[derive(Debug)]
struct Inner {
    a_u8: i64,
}

fn main() {}
