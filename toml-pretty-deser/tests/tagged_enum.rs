use std::{cell::RefCell, rc::Rc};
use toml_pretty_deser::{
    AnnotatedError, AsEnum, AsNested, AsTaggedEnum, DeserError, FieldMatchMode, FromTomlTable,
    StringNamedEnum, ToConcrete, TomlHelper, TomlValue, TomlValueState, VerifyFromToml,
    deserialize, deserialize_with_mode, make_partial, make_partial_enum,
};

#[make_partial]
#[derive(Debug)]
struct InnerA {
    n: i32,
    o: u32,
}

#[make_partial]
#[derive(Debug)]
struct InnerB {
    s: u32,
    t: u32,
}

#[make_partial_enum] // creates PartialEitherOne { KindA(PartialInnerA, ...)}
#[derive(Debug)]
enum EitherOne {
    KindA(InnerA),
    KindB(InnerB),
}

#[make_partial]
#[derive(Debug)]
struct OuterEither {
    #[enum_tagged("kind")]
    choice: EitherOne,
}

#[test]
fn test_either_one_happy_a() {
    let toml = "
    [choice]
        kind = 'KindA'
        n = -5
        o = 1
    ";
    let result: Result<_, _> =
        deserialize_with_mode::<PartialOuterEither, OuterEither>(toml, FieldMatchMode::Exact);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        match output.choice {
            EitherOne::KindA(inner) => {
                assert_eq!(inner.n, -5);
                assert_eq!(inner.o, 1);
            }
            EitherOne::KindB(_) => {
                panic!("expected KindA variant");
            }
        }
    }
}

#[test]
fn test_either_one_happy_b() {
    let toml = "
    choice = {
        kind = 'KindB',
        s = 5,
        t = 0
    }
    ";
    let result: Result<_, _> =
        deserialize_with_mode::<PartialOuterEither, OuterEither>(toml, FieldMatchMode::Exact);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        match output.choice {
            EitherOne::KindA(_) => {
                panic!("expected KindB variant");
            }
            EitherOne::KindB(inner) => {
                assert_eq!(inner.s, 5);
                assert_eq!(inner.t, 0);
            }
        }
    }
}
