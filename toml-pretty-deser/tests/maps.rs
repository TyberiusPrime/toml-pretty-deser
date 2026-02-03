use std::{cell::RefCell, collections::HashMap, rc::Rc};
use toml_pretty_deser::{
    AnnotatedError, AsOptionalTaggedEnum, AsTaggedEnum, AsVecTaggedEnum, DeserError,
    FieldMatchMode, FromTomlTable, StringNamedEnum, ToConcrete, TomlHelper, TomlValue,
    TomlValueState, VerifyFromToml, deserialize, deserialize_with_mode, make_partial,
    make_partial_enum,
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

#[derive(StringNamedEnum, Debug, Clone, PartialEq, Eq)]
enum ByString {
    AlphaBeta,
    GammaDelta,
}

#[make_partial]
#[derive(Debug)]
struct Inner {
    n: u8,
}

#[make_partial]
#[derive(Debug)]
struct Mapped {
    mapped_u8: HashMap<String, u8>,
    mapped_enum: HashMap<String, ByString>,
    mapped_either: HashMap<String, EitherOne>,
    mapped_struct: HashMap<String, Inner>,
    mapped_vec_string: HashMap<String, Vec<String>>,
    mapped_vec_enum: HashMap<String, Vec<ByString>>,
    mapped_vec_either: HashMap<String, Vec<EitherOne>>,
    mapped_vec_struct: HashMap<String, Vec<Inner>>,
    opt_mapped_u8: Option<HashMap<String, u8>>,
    opt_mapped_enum: Option<HashMap<String, ByString>>,
    opt_mapped_either: Option<HashMap<String, EitherOne>>,
    opt_mapped_struct: Option<HashMap<String, Inner>>,
}

#[test]
fn test_mapped_happy() {
    let toml = "
        [mapped_u8] 
            a = 1
            b = 2
        [mapped_enum]
            a = 'AlphaBeta'
            b = 'GammaDelta'
        [mapped_either]
            a = { KindA = { n = 10, o = 20 } }
            b = { KindB = { s = 30, t = 40 } }
        [mapped_struct]
            a = { n = 5 }
        [mapped_vec_string]
            a = ['hello','world']
        [mapped_vec_enum]
                a = ['AlphaBeta','GammaDelta']
        [mapped_vec_either]
            a = [ { KindA = { n = 1, o = 2 } }, { KindB = { s = 3, t = 4 } } ]
        [mapped_vec_struct]
            a = [ { n = 5 }, { n = 6 } ]
        [opt_mapped_u8]
            a = 10
            b = 20
        [opt_mapped_enum]
            a = 'AlphaBeta'
            b = 'GammaDelta'
        [opt_mapped_either]
            a = { KindA = { n = 100, o = 200 } }
            b = { KindB = { s = 300, t = 400 } }
        [opt_mapped_struct]
            a = { n = 50 }

    ";
    let result: Result<_, _> = deserialize::<PartialMapped, Mapped>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.mapped_u8.get("a"), Some(&1));
        assert_eq!(output.mapped_enum.get("b"), Some(&ByString::GammaDelta));
        assert!(matches!(
            output.mapped_either.get("a"),
            Some(EitherOne::KindA(inner)) if inner.n == 10 && inner.o == 20
        ));
        assert!(matches!(
            output.mapped_either.get("b"),
            Some(EitherOne::KindB(inner)) if inner.s == 30 && inner.t == 40
        ));
        assert!(matches!(
            output.mapped_struct.get("a"),
            Some(inner) if inner.n == 5
        ));
        assert_eq!(output.mapped_u8.len(), 2);
        assert_eq!(output.mapped_enum.len(), 2);
        assert_eq!(output.mapped_either.len(), 2);
        assert_eq!(output.mapped_struct.len(), 1);

        assert_eq!(output.opt_mapped_u8.as_ref().unwrap().get("a"), Some(&1));
        assert_eq!(
            output.opt_mapped_enum.as_ref().unwrap().get("b"),
            Some(&ByString::GammaDelta)
        );
        assert!(matches!(
            output.opt_mapped_either.as_ref().unwrap().get("a"),
            Some(EitherOne::KindA(inner)) if inner.n == 10 && inner.o == 20
        ));
        assert!(matches!(
            output.opt_mapped_either.as_ref().unwrap().get("b"),
            Some(EitherOne::KindB(inner)) if inner.s == 30 && inner.t == 40
        ));
        assert!(matches!(
            output.opt_mapped_struct.as_ref().unwrap().get("a"),
            Some(inner) if inner.n == 5
        ));
        assert_eq!(output.opt_mapped_u8.as_ref().unwrap().len(), 2);
        assert_eq!(output.opt_mapped_enum.as_ref().unwrap().len(), 2);
        assert_eq!(output.opt_mapped_either.as_ref().unwrap().len(), 2);
        assert_eq!(output.opt_mapped_struct.as_ref().unwrap().len(), 1);

        assert_eq!(output.mapped_vec_string.get("a").unwrap(), &vec!["hello".to_string(), "world".to_string()]);
        assert_eq!(output.mapped_vec_enum.get("a").unwrap(), &vec![ByString::AlphaBeta, ByString::GammaDelta]);
        assert_eq!(output.mapped_vec_either.get("a").unwrap().len(), 2);
        assert!(matches!(
            &output.mapped_vec_either.get("a").unwrap()[0],
            EitherOne::KindA(inner) if inner.n == 1 && inner.o == 2
        ));
        assert_eq!(output.mapped_vec_struct.get("a").unwrap().len(), 2);
        assert_eq!(output.mapped_vec_struct.get("a").unwrap()[0].n, 5);
        assert_eq!(output.mapped_vec_struct.get("a").unwrap()[1].n, 6);
    }
}
