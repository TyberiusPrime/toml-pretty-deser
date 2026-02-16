use toml_pretty_deser::prelude::*;

#[tpd(root, no_verify)]
struct MyStruct {
    a: u8
}

#[tpd(root, tag="kind")]
#[derive(Debug)]
enum TN {
    TagA (MyStruct),
    TagB (MyStruct)

}

fn main() {
}
