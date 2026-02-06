# toml-pretty-deser-macros

[Documentation](https://docs.rs/toml-pretty-deser-macros) | [crates.io](https://crates.io/crates/toml-pretty-deser-macros)

Macro crate for [toml-pretty-deser](https://docs.rs/toml-pretty-deser) — provides derive macros for TOML deserialization with rich, helpful error messages.

## Overview

This crate provides procedural macros that work with the `toml-pretty-deser` library to deserialize TOML config files while providing detailed, user-friendly error messages with suggestions, spans, and context.

## Features

- **`#[tdp]`** — Unified macro that automatically detects and handles:
  - Structs with named fields (generates Partial structs for deserialization)
  - Unit enums (enables use in configs)
  - Tagged enums with a discriminator key

## Example

```rust
use toml_pretty_deser_macros::tdp;

#[tdp]
#[derive(Debug, Clone)]
enum Color {
    Red,
    Green,
    Blue,
}

#[tdp]
struct Config {
    color: Color,
    name: String,
}
```

### Tagged Enums

```rust
#[tdp(tag = "kind", aliases = ["type"])]
#[derive(Debug)]
enum EitherOne {
    KindA(InnerA),
    KindB(InnerB),
}
```

### Structs Without Verification

```rust
#[tdp(partial = false)]
struct Config {
    name: String,
    count: u32,
}
```

## License

See main [toml-pretty-deser](https://github.com/anomalyco/toml-pretty-deser) repository for license information.
