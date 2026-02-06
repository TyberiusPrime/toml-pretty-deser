#![allow(clippy::struct_field_names)]
use indexmap::IndexMap;
use toml_pretty_deser::prelude::*;

#[tdp]
#[derive(Debug)]
struct InnerA {
    n: i32,
    o: u32,
}

#[tdp]
#[derive(Debug)]
struct InnerB {
    s: u32,
    t: u32,
}

#[tdp(tag = "kind")] // creates PartialEitherOne with TaggedEnumMeta
#[derive(Debug)]
enum EitherOne {
    KindA(InnerA),
    KindB(InnerB),
}

#[tdp]
#[derive(Debug, Clone, PartialEq, Eq)]
enum ByString {
    AlphaBeta,
    GammaDelta,
}

#[tdp]
#[derive(Debug, Clone)]
struct Inner {
    n: u8,
}

#[tdp]
#[derive(Debug)]
struct Mapped {
    mapped_u8: IndexMap<String, u8>,
    mapped_enum: IndexMap<String, ByString>,
    #[enum_tagged]
    mapped_either: IndexMap<String, EitherOne>,
    #[nested]
    mapped_struct: IndexMap<String, Inner>,
    mapped_vec_string: IndexMap<String, Vec<String>>,
    mapped_vec_enum: IndexMap<String, Vec<ByString>>,
    #[enum_tagged]
    mapped_vec_either: IndexMap<String, Vec<EitherOne>>,
    #[nested]
    mapped_vec_struct: IndexMap<String, Vec<Inner>>,
    opt_mapped_u8: Option<IndexMap<String, u8>>,
    opt_mapped_enum: Option<IndexMap<String, ByString>>,
    #[enum_tagged]
    opt_mapped_either: Option<IndexMap<String, EitherOne>>,
    #[nested]
    opt_mapped_struct: Option<IndexMap<String, Inner>>,

    opt_mapped_vec_u8: Option<IndexMap<String, Vec<u8>>>,
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
            a = { kind = 'KindA', n = 10, o = 20 }
            b = { kind = 'KindB', s = 30, t = 40 }
        [mapped_struct]
            a = { n = 5 }
        [mapped_vec_string]
            a = ['hello','world']
        [mapped_vec_enum]
                a = ['AlphaBeta','GammaDelta']
        [mapped_vec_either]
            a = [ { kind = 'KindA', n = 1, o = 2 }, { kind = 'KindB', s = 3, t = 4 } ]
        [mapped_vec_struct]
            a = [ { n = 5 }, { n = 6 } ]
        [opt_mapped_u8]
            a = 10
            b = 20
        [opt_mapped_enum]
            a = 'AlphaBeta'
            b = 'GammaDelta'
        [opt_mapped_either]
            a = { kind = 'KindA', n = 100, o = 200 }
            b = { kind = 'KindB', s = 300, t = 400 }
        [opt_mapped_struct]
            a = { n = 50 }
        [opt_mapped_vec_u8]
            a = [100,200,255]
    

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

        assert_eq!(output.opt_mapped_u8.as_ref().unwrap().get("a"), Some(&10));
        assert_eq!(
            output.opt_mapped_enum.as_ref().unwrap().get("b"),
            Some(&ByString::GammaDelta)
        );
        assert!(matches!(
            output.opt_mapped_either.as_ref().unwrap().get("a"),
            Some(EitherOne::KindA(inner)) if inner.n == 100 && inner.o == 200
        ));
        assert!(matches!(
            output.opt_mapped_either.as_ref().unwrap().get("b"),
            Some(EitherOne::KindB(inner)) if inner.s == 300 && inner.t == 400
        ));
        assert!(matches!(
            output.opt_mapped_struct.as_ref().unwrap().get("a"),
            Some(inner) if inner.n == 50
        ));
        assert_eq!(output.opt_mapped_u8.as_ref().unwrap().len(), 2);
        assert_eq!(output.opt_mapped_enum.as_ref().unwrap().len(), 2);
        assert_eq!(output.opt_mapped_either.as_ref().unwrap().len(), 2);
        assert_eq!(output.opt_mapped_struct.as_ref().unwrap().len(), 1);

        assert_eq!(
            output.mapped_vec_string.get("a").unwrap(),
            &vec!["hello".to_string(), "world".to_string()]
        );
        assert_eq!(
            output.mapped_vec_enum.get("a").unwrap(),
            &vec![ByString::AlphaBeta, ByString::GammaDelta]
        );
        assert_eq!(output.mapped_vec_either.get("a").unwrap().len(), 2);
        assert!(matches!(
            &output.mapped_vec_either.get("a").unwrap()[0],
            EitherOne::KindA(inner) if inner.n == 1 && inner.o == 2
        ));
        assert_eq!(output.mapped_vec_struct.get("a").unwrap().len(), 2);
        assert_eq!(output.mapped_vec_struct.get("a").unwrap()[0].n, 5);
        assert_eq!(output.mapped_vec_struct.get("a").unwrap()[1].n, 6);
        assert_eq!(
            output.opt_mapped_vec_u8.as_ref().unwrap().get("a").unwrap(),
            &vec![100, 200, 255]
        );
    }
}

#[test]
fn test_mapped_happy_allow_single() {
    let toml = "
        [mapped_u8] 
            a = 1
            b = 2
        [mapped_enum]
            a = 'AlphaBeta'
            b = 'GammaDelta'
        [mapped_either]
            a = { kind = 'KindA', n = 10, o = 20 }
            b = { kind = 'KindB', s = 30, t = 40 }
        [mapped_struct]
            a = { n = 5 }
        [mapped_vec_string]
            a = 'hello'
        [mapped_vec_enum]
                a = 'AlphaBeta'
        [mapped_vec_either]
            a = { kind = 'KindA', n = 1, o = 2 }
        [mapped_vec_struct]
            a = { n = 5 }
        [opt_mapped_u8]
            a = 10
            b = 20
        [opt_mapped_enum]
            a = 'AlphaBeta'
            b = 'GammaDelta'
        [opt_mapped_either]
            a = { kind = 'KindA', n = 100, o = 200 }
            b = { kind = 'KindB', s = 300, t = 400 }
        [opt_mapped_struct]
            a = { n = 50 }
        [opt_mapped_vec_u8]
            a = 100
    ";
    let result: Result<_, _> = deserialize_with_mode::<PartialMapped, Mapped>(
        toml,
        FieldMatchMode::Exact,
        VecMode::SingleOk,
    );
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

        assert_eq!(output.opt_mapped_u8.as_ref().unwrap().get("a"), Some(&10));
        assert_eq!(
            output.opt_mapped_enum.as_ref().unwrap().get("b"),
            Some(&ByString::GammaDelta)
        );
        assert!(matches!(
            output.opt_mapped_either.as_ref().unwrap().get("a"),
            Some(EitherOne::KindA(inner)) if inner.n == 100 && inner.o == 200
        ));
        assert!(matches!(
            output.opt_mapped_either.as_ref().unwrap().get("b"),
            Some(EitherOne::KindB(inner)) if inner.s == 300 && inner.t == 400
        ));
        assert!(matches!(
            output.opt_mapped_struct.as_ref().unwrap().get("a"),
            Some(inner) if inner.n == 50
        ));
        assert_eq!(output.opt_mapped_u8.as_ref().unwrap().len(), 2);
        assert_eq!(output.opt_mapped_enum.as_ref().unwrap().len(), 2);
        assert_eq!(output.opt_mapped_either.as_ref().unwrap().len(), 2);
        assert_eq!(output.opt_mapped_struct.as_ref().unwrap().len(), 1);

        assert_eq!(
            output.mapped_vec_string.get("a").unwrap(),
            &vec!["hello".to_string()]
        );
        assert_eq!(
            output.mapped_vec_enum.get("a").unwrap(),
            &vec![ByString::AlphaBeta]
        );
        assert_eq!(output.mapped_vec_either.get("a").unwrap().len(), 1);
        assert!(matches!(
            &output.mapped_vec_either.get("a").unwrap()[0],
            EitherOne::KindA(inner) if inner.n == 1 && inner.o == 2
        ));
        assert_eq!(output.mapped_vec_struct.get("a").unwrap().len(), 1);
        assert_eq!(output.mapped_vec_struct.get("a").unwrap()[0].n, 5);
        assert_eq!(
            output.opt_mapped_vec_u8.as_ref().unwrap().get("a").unwrap(),
            &vec![100]
        );
    }
}
#[test]
#[allow(clippy::too_many_lines)]
fn test_mapped_happy_inline() {
    let toml = "
        mapped_u8 = {
            a = 1,
            b = 2
        }
        mapped_enum = {
            a = 'AlphaBeta',
            b = 'GammaDelta'
        }
        mapped_either = {
            a = { kind = 'KindA', n = 10, o = 20 },
            b = { kind = 'KindB', s = 30, t = 40 },
        }
        mapped_struct = {
            a = { n = 5 }
        }
        mapped_vec_string = {
            a = ['hello','world'] 
        }
        mapped_vec_enum = {
               a = ['AlphaBeta','GammaDelta'  ]
        }
        mapped_vec_either = {
           a = [ { kind = 'KindA', n = 1, o = 2 }, { kind = 'KindB', s = 3, t = 4 } ] 
        }
        mapped_vec_struct = {
           a = [ { n = 5 }, { n = 6 }  ]
        }
        opt_mapped_u8 = {
           a = 10,
           b = 20
        }
        opt_mapped_enum = {
           a = 'AlphaBeta',
           b = 'GammaDelta'
        }
        opt_mapped_either = {
           a = { kind = 'KindA', n = 100, o = 200 },
           b = { kind = 'KindB', s = 300, t = 400 }
        }
        opt_mapped_struct = {
            a = { n = 50 }
        }

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

        assert_eq!(output.opt_mapped_u8.as_ref().unwrap().get("a"), Some(&10));
        assert_eq!(
            output.opt_mapped_enum.as_ref().unwrap().get("b"),
            Some(&ByString::GammaDelta)
        );
        assert!(matches!(
            output.opt_mapped_either.as_ref().unwrap().get("a"),
            Some(EitherOne::KindA(inner)) if inner.n == 100 && inner.o == 200
        ));
        assert!(matches!(
            output.opt_mapped_either.as_ref().unwrap().get("b"),
            Some(EitherOne::KindB(inner)) if inner.s == 300 && inner.t == 400
        ));
        assert!(matches!(
            output.opt_mapped_struct.as_ref().unwrap().get("a"),
            Some(inner) if inner.n == 50
        ));
        assert_eq!(output.opt_mapped_u8.as_ref().unwrap().len(), 2);
        assert_eq!(output.opt_mapped_enum.as_ref().unwrap().len(), 2);
        assert_eq!(output.opt_mapped_either.as_ref().unwrap().len(), 2);
        assert_eq!(output.opt_mapped_struct.as_ref().unwrap().len(), 1);

        assert_eq!(
            output.mapped_vec_string.get("a").unwrap(),
            &vec!["hello".to_string(), "world".to_string()]
        );
        assert_eq!(
            output.mapped_vec_enum.get("a").unwrap(),
            &vec![ByString::AlphaBeta, ByString::GammaDelta]
        );
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
#[test]
fn test_mapped_missing() {
    let toml = "
        [mapped_u8]
            a = 1
        # mapped_enum missing
        # mapped_either missing
        # mapped_struct missing
    ";
    let result: Result<_, _> = deserialize::<PartialMapped, Mapped>(toml);
    if let Err(DeserError::DeserFailure(errors, _)) = result {
        assert!(errors.len() >= 3);
        assert_eq!(
            errors[0].inner.spans[0].msg,
            "Missing required key: 'mapped_enum'."
        );
        assert_eq!(
            errors[0].inner.help,
            Some("This key is required but was not found in the TOML document.".to_string())
        );
    } else {
        panic!("Expected failure due to missing required fields");
    }
}

#[test]
fn test_mapped_empty_map() {
    let toml = "
        [mapped_u8]
        [mapped_enum]
        [mapped_either]
        [mapped_struct]
        [mapped_vec_string]
        [mapped_vec_enum]
        [mapped_vec_either]
        [mapped_vec_struct]
    ";
    let result: Result<_, _> = deserialize::<PartialMapped, Mapped>(toml);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.mapped_u8.len(), 0);
        assert_eq!(output.mapped_enum.len(), 0);
        assert_eq!(output.mapped_either.len(), 0);
        assert_eq!(output.mapped_struct.len(), 0);
        assert_eq!(output.mapped_vec_string.len(), 0);
        assert_eq!(output.mapped_vec_enum.len(), 0);
        assert_eq!(output.mapped_vec_either.len(), 0);
        assert_eq!(output.mapped_vec_struct.len(), 0);
        assert!(output.opt_mapped_u8.as_ref().is_none());
        assert!(output.opt_mapped_enum.as_ref().is_none());
        assert!(output.opt_mapped_either.as_ref().is_none());
        assert!(output.opt_mapped_struct.as_ref().is_none());
    }
}

#[test]
fn test_mapped_wrong_type_primitive() {
    let toml = "
        [mapped_u8]
            a = 'not a number'
    ";
    let result: Result<_, _> = deserialize::<PartialMapped, Mapped>(toml);
    if let Err(DeserError::DeserFailure(errors, _)) = result {
        assert!(errors
            .iter()
            .any(|e| e.inner.spans[0].msg.contains("Wrong type")));
    } else {
        panic!("Expected failure due to wrong type");
    }
}

#[test]
fn test_mapped_wrong_type_enum() {
    let toml = "
        [mapped_enum]
            a = 123
    ";
    let result: Result<_, _> = deserialize::<PartialMapped, Mapped>(toml);
    if let Err(DeserError::DeserFailure(errors, _)) = result {
        assert!(errors
            .iter()
            .any(|e| e.inner.spans[0].msg.contains("Wrong type")));
    } else {
        panic!("Expected failure due to wrong type for enum");
    }
}

#[test]
fn test_mapped_invalid_enum_variant() {
    let toml = "
        [mapped_enum]
            a = 'InvalidVariant'
    ";
    let result: Result<_, _> = deserialize::<PartialMapped, Mapped>(toml);
    if let Err(DeserError::DeserFailure(errors, _)) = result {
        assert!(errors
            .iter()
            .any(|e| e.inner.spans[0].msg.contains("Invalid enum variant")));
    } else {
        panic!("Expected failure due to invalid enum variant");
    }
}

#[test]
fn test_mapped_vec_enum_invalid_variant() {
    let toml = "
        [mapped_vec_enum]
            a = ['AlphaBeta', 'InvalidVariant']
    ";
    let result: Result<_, _> = deserialize::<PartialMapped, Mapped>(toml);
    dbg!(&result);
    if let Err(e) = result {
        insta::assert_snapshot!(e.pretty("test.toml"));
    } else {
        panic!("Expected failure due to invalid enum variant in vec");
    }
}

#[test]
fn test_mapped_tagged_enum_unknown_variant() {
    let toml = "
        [mapped_u8]
            a = 1
        [mapped_enum]
            a = 'AlphaBeta'
        [mapped_either]
            a = { kind = 'KindX', n = 10, o = 20 }
        [mapped_struct]
            a = { n = 5 }
        [mapped_vec_string]
            a = ['hello']
        [mapped_vec_enum]
            a = ['AlphaBeta']
        [mapped_vec_either]
            a = [{ kind = 'KindA', n = 1, o = 2 }]
        [mapped_vec_struct]
            a = [{ n = 5 }]
    ";
    let result: Result<_, _> = deserialize::<PartialMapped, Mapped>(toml);
    if let Err(DeserError::DeserFailure(errors, _)) = result {
        for e in &errors {
            eprintln!("Error: {}", e.inner.spans[0].msg);
        }
        assert!(errors
            .iter()
            .any(|e| { e.inner.spans[0].msg.contains("Unknown enum variant") }));
    } else {
        panic!("Expected failure due to unknown tagged enum variant");
    }
}

#[test]
fn test_mapped_tagged_enum_missing_variant_field() {
    let toml = "
        [mapped_either]
            a = { kind = 'KindA', n = 10 }
    ";
    let result: Result<_, _> = deserialize::<PartialMapped, Mapped>(toml);
    if let Err(DeserError::DeserFailure(errors, _)) = result {
        assert!(errors
            .iter()
            .any(|e| e.inner.spans[0].msg == "Missing required key: 'o'."));
    } else {
        panic!("Expected failure due to missing variant field");
    }
}

#[test]
fn test_mapped_nested_missing_field() {
    let toml = "
        [mapped_struct]
            a = { }  # missing n field
    ";
    let result: Result<_, _> = deserialize::<PartialMapped, Mapped>(toml);
    dbg!(&result);
    if let Err(DeserError::DeserFailure(errors, _)) = result {
        let expected = &[
            "Missing required key: 'n'.",
            "Missing required key: 'mapped_u8'.",
            "Missing required key: 'mapped_enum'.",
            "Missing required key: 'mapped_either'.",
            "Missing required key: 'mapped_vec_string'.",
            "Missing required key: 'mapped_vec_enum'.",
            "Missing required key: 'mapped_vec_either'.",
            "Missing required key: 'mapped_vec_struct'.",
        ];
        for exp in expected {
            assert!(errors.iter().any(|e| e.inner.spans[0].msg == *exp));
        }
        assert!(errors.iter().any(|e| {
            let pretty = e.pretty("test.toml");
            pretty
                == "  ╭─test.toml
  ┆
2 │         [mapped_struct]
3 │             a = { }  # missing n field
  ┆                 ─┬─                   
  ┆                  │                    
  ┆                  ╰───────────────────── Missing required key: 'n'.
──╯
Hint: This key is required but was not found in the TOML document.
"
        }));
    } else {
        panic!("Expected failure due to missing nested field");
    }
}

#[test]
fn test_mapped_vec_struct_missing_field() {
    let toml = "
        [mapped_vec_struct]
            a = [ { n = 5 }, { } ]  # second element missing n
    ";
    let result: Result<_, _> = deserialize::<PartialMapped, Mapped>(toml);
    if let Err(DeserError::DeserFailure(errors, output)) = result {
        dbg!(&output);
        assert!(errors
            .iter()
            .any(|e| e.inner.spans[0].msg == "Missing required key: 'n'."));
    } else {
        panic!("Expected failure due to missing field in vec struct");
    }
}

#[test]
fn test_mapped_vec_string_wrong_element_type() {
    let toml = "
        [mapped_u8]
            a = 1
        [mapped_enum]
            a = 'AlphaBeta'
        [mapped_either]
            a = { kind = 'KindA', n = 10, o = 20 }
        [mapped_struct]
            a = { n = 5 }
        [mapped_vec_string]
            a = [1, 2, 3]
        [mapped_vec_enum]
            a = ['AlphaBeta']
        [mapped_vec_either]
            a = [{ kind = 'KindA', n = 1, o = 2 }]
        [mapped_vec_struct]
            a = [{ n = 5 }]
    ";
    let result: Result<_, _> = deserialize::<PartialMapped, Mapped>(toml);
    assert!(result.is_err());
    if let Err(DeserError::DeserFailure(errors, _)) = result {
        assert!(!errors.is_empty());
    }
}

#[test]
fn test_mapped_vec_either_wrong_element_type() {
    let toml = "
        [mapped_u8]
            a = 1
        [mapped_enum]
            a = 'AlphaBeta'
        [mapped_either]
            a = { kind = 'KindA', n = 10, o = 20 }
        [mapped_struct]
            a = { n = 5 }
        [mapped_vec_string]
            a = ['hello']
        [mapped_vec_enum]
            a = ['AlphaBeta']
        [mapped_vec_either]
            a = [1, 2, 3]
        [mapped_vec_struct]
            a = [{ n = 5 }]
    ";
    let result: Result<_, _> = deserialize::<PartialMapped, Mapped>(toml);
    assert!(result.is_err());
    if let Err(DeserError::DeserFailure(errors, _)) = result {
        assert!(!errors.is_empty());
    }
}

#[test]
fn test_mapped_optional_all_present() {
    let toml = "
        [mapped_u8]
            a = 1
        [mapped_enum]
            a = 'AlphaBeta'
        [mapped_either]
            a = { kind = 'KindA', n = 10, o = 20 }
        [mapped_struct]
            a = { n = 5 }
        [mapped_vec_string]
            a = ['hello']
        [mapped_vec_enum]
            a = ['AlphaBeta']
        [mapped_vec_either]
            a = [{ kind = 'KindA', n = 1, o = 2 }]
        [mapped_vec_struct]
            a = [{ n = 5 }]
        [opt_mapped_u8]
            a = 10
        [opt_mapped_enum]
            a = 'AlphaBeta'
        [opt_mapped_either]
            a = { kind = 'KindA', n = 100, o = 200 }
        [opt_mapped_struct]
            a = { n = 50 }
    ";
    let result: Result<_, _> = deserialize::<PartialMapped, Mapped>(toml);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert!(output.opt_mapped_u8.is_some());
        assert!(output.opt_mapped_enum.is_some());
        assert!(output.opt_mapped_either.is_some());
        assert!(output.opt_mapped_struct.is_some());
    }
}

#[test]
fn test_mapped_optional_all_missing() {
    let toml = "
        [mapped_u8]
            a = 1
        [mapped_enum]
            a = 'AlphaBeta'
        [mapped_either]
            a = { kind = 'KindA', n = 10, o = 20 }
        [mapped_struct]
            a = { n = 5 }
        [mapped_vec_string]
            a = ['hello']
        [mapped_vec_enum]
            a = ['AlphaBeta']
        [mapped_vec_either]
            a = [{ kind = 'KindA', n = 1, o = 2 }]
        [mapped_vec_struct]
            a = [{ n = 5 }]
    ";
    let result: Result<_, _> = deserialize::<PartialMapped, Mapped>(toml);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert!(output.opt_mapped_u8.is_none());
        assert!(output.opt_mapped_enum.is_none());
        assert!(output.opt_mapped_either.is_none());
        assert!(output.opt_mapped_struct.is_none());
    }
}

#[test]
fn test_mapped_optional_wrong_type() {
    let toml = "
        [mapped_u8]
            a = 1
        [mapped_enum]
            a = 'AlphaBeta'
        [mapped_either]
            a = { kind = 'KindA', n = 10, o = 20 }
        [mapped_struct]
            a = { n = 5 }
        [mapped_vec_string]
            a = ['hello']
        [mapped_vec_enum]
            a = ['AlphaBeta']
        [mapped_vec_either]
            a = [{ kind = 'KindA', n = 1, o = 2 }]
        [mapped_vec_struct]
            a = [{ n = 5 }]
        [opt_mapped_u8]
            a = 'wrong'
    ";
    let result: Result<_, _> = deserialize::<PartialMapped, Mapped>(toml);
    if let Err(DeserError::DeserFailure(errors, _)) = result {
        assert!(errors
            .iter()
            .any(|e| e.inner.spans[0].msg.contains("Wrong type")));
    } else {
        panic!("Expected failure due to wrong type in optional map");
    }
}

#[test]
fn test_mapped_nested_unknown_key() {
    let toml = "
        [mapped_struct]
            a = { n = 5, unknown = 99 }
    ";
    let result: Result<_, _> = deserialize::<PartialMapped, Mapped>(toml);
    if let Err(DeserError::DeserFailure(errors, _)) = result {
        assert!(errors
            .iter()
            .any(|e| e.inner.spans[0].msg == "Unknown key."));
    } else {
        panic!("Expected failure due to unknown key in nested struct");
    }
}

#[test]
fn test_mapped_tagged_enum_unknown_key_in_variant() {
    let toml = "
        [mapped_either]
            a = { kind = 'KindA', n = 10, o = 20, unknown = 99 }
    ";
    let result: Result<_, _> = deserialize::<PartialMapped, Mapped>(toml);
    if let Err(DeserError::DeserFailure(errors, _)) = result {
        assert!(errors
            .iter()
            .any(|e| e.inner.spans[0].msg == "Unknown key."));
    } else {
        panic!("Expected failure due to unknown key in tagged enum variant");
    }
}

#[test]
fn test_mapped_struct_wrong_field_type() {
    let toml = "
        [mapped_u8]
            a = 1
        [mapped_enum]
            a = 'AlphaBeta'
        [mapped_either]
            a = { kind = 'KindA', n = 10, o = 20 }
        [mapped_struct]
            a = { n = 'not a number' }
        [mapped_vec_string]
            a = ['hello']
        [mapped_vec_enum]
            a = ['AlphaBeta']
        [mapped_vec_either]
            a = [{ kind = 'KindA', n = 1, o = 2 }]
        [mapped_vec_struct]
            a = [{ n = 5 }]
    ";
    let result: Result<_, _> = deserialize::<PartialMapped, Mapped>(toml);
    assert!(result.is_err());
    if let Err(DeserError::DeserFailure(errors, _)) = result {
        insta::assert_snapshot!(errors[0].pretty("test.toml"));
    } else {
        panic!("expected Deserfailure")
    }
}

#[test]
fn test_mapped_vec_either_empty_vec() {
    let toml = "
        [mapped_u8]
            a = 1
        [mapped_enum]
            a = 'AlphaBeta'
        [mapped_either]
            a = { kind = 'KindA', n = 10, o = 20 }
        [mapped_struct]
            a = { n = 5 }
        [mapped_vec_string]
            a = ['hello']
        [mapped_vec_enum]
            a = ['AlphaBeta']
        [mapped_vec_either]
            a = []
        [mapped_vec_struct]
            a = [{ n = 5 }]
    ";
    let result: Result<_, _> = deserialize::<PartialMapped, Mapped>(toml);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.mapped_vec_either.get("a").unwrap().len(), 0);
    }
}

#[test]
fn test_mapped_mixed_valid_invalid() {
    let toml = "
        [mapped_u8]
            a = 1
            b = 'wrong'
            c = 3
    ";
    let result: Result<_, _> = deserialize::<PartialMapped, Mapped>(toml);
    if let Err(DeserError::DeserFailure(errors, _)) = result {
        assert!(errors
            .iter()
            .any(|e| e.inner.spans[0].msg.contains("Wrong type")));
    } else {
        panic!("Expected failure due to mixed valid/invalid entries");
    }
}

#[test]
fn test_mapped_optional_struct_missing_field() {
    let toml = "
        [opt_mapped_struct]
            a = { }  # missing n field
    ";
    let result: Result<_, _> = deserialize::<PartialMapped, Mapped>(toml);
    if let Err(DeserError::DeserFailure(errors, _)) = result {
        assert!(errors
            .iter()
            .any(|e| e.inner.spans[0].msg == "Missing required key: 'n'."));
    } else {
        panic!("Expected failure due to missing field in optional struct");
    }
}

#[tdp]
#[derive(Debug)]
struct Barcodes {
    barcodes: IndexMap<String, String>,
}

#[test]
fn test_map_order_retained() {
    let toml = "
        [barcodes] 
            alpha = 'agtc'
            beta = 'cccc'
            gamma = 'c'
            delta = 'c'
            epsilon = 'c'
            romeo = 'c'
            tango = 'c'
            juliet = 'c'
    ";
    let result: Result<_, _> = deserialize::<PartialBarcodes, Barcodes>(toml);
    assert!(result.is_ok());
    if let Ok(output) = result {
        let keys: Vec<&String> = output.barcodes.keys().collect();
        let should: Vec<String> = [
            "alpha", "beta", "gamma", "delta", "epsilon", "romeo", "tango", "juliet",
        ]
        .iter()
        .map(ToString::to_string)
        .collect();
        let actual: Vec<String> = keys.iter().map(ToString::to_string).collect();
        assert_eq!(actual, should);
    }
}

#[derive(Debug, Clone)]
#[allow(clippy::upper_case_acronyms)]
struct DNA(String);

#[tdp]
#[derive(Debug)]
struct BarcodesValidated {
    barcodes: IndexMap<String, DNA>,
}

impl PartialEq<DNA> for DNA {
    fn eq(&self, other: &DNA) -> bool {
        self.0 == other.0
    }
}

impl FromTomlItem for DNA {
    fn from_toml_item(
        item: &toml_edit::Item,
        parent_span: std::ops::Range<usize>,
        _col: &TomlCollector,
    ) -> TomlValue<DNA> {
        match item.as_str() {
            Some(s) => {
                if s.chars()
                    .all(|c| matches!(c, 'a' | 'c' | 'g' | 't' | 'A' | 'C' | 'G' | 'T'))
                {
                    TomlValue::new_ok(DNA(s.to_string()), parent_span)
                } else {
                    TomlValue::new_validation_failed(
                        item.span().unwrap_or(parent_span),
                        "Invalid base".to_string(),
                        Some("Use only AGTC".to_string()),
                    )
                }
            }
            None => TomlValue::new_wrong_type(item, parent_span, "String(DNA)"),
        }
    }
}

#[test]
fn test_map_validate_elements() {
    let toml = "
        [barcodes] 
            alpha = 'agtc'
            beta = 'ccGc'
    ";
    let result: Result<_, _> = deserialize::<PartialBarcodesValidated, BarcodesValidated>(toml);
    assert!(result.is_ok());
    if let Ok(output) = result {
        let keys: Vec<&String> = output.barcodes.keys().collect();
        let should: Vec<String> = ["alpha", "beta"].iter().map(ToString::to_string).collect();
        let actual: Vec<String> = keys.iter().map(ToString::to_string).collect();
        assert_eq!(actual, should);
        let actual: Vec<DNA> = output.barcodes.values().cloned().collect();
        let should = vec![DNA("agtc".to_string()), DNA("ccGc".to_string())];
        assert_eq!(actual, should);
    }
}

#[tdp]
#[derive(Debug)]
struct BarcodesMapVec {
    barcodes: IndexMap<String, Vec<DNA>>,
}
#[test]
fn test_map_map_vec() {
    let toml = "
        [barcodes] 
            alpha = ['agtc', 'gc']
            beta = 'ccGc'
    ";
    let result: Result<_, _> = deserialize_with_mode::<PartialBarcodesMapVec, BarcodesMapVec>(
        toml,
        FieldMatchMode::Exact,
        VecMode::SingleOk,
    );
    assert!(result.is_ok());
    if let Ok(output) = result {
        let keys: Vec<&String> = output.barcodes.keys().collect();
        let should: Vec<String> = ["alpha", "beta"].iter().map(ToString::to_string).collect();
        let actual: Vec<String> = keys.iter().map(ToString::to_string).collect();
        assert_eq!(actual, should);
        let actual: Vec<Vec<DNA>> = output.barcodes.values().cloned().collect();
        let should = vec![
            vec![DNA("agtc".to_string()), DNA("gc".to_string())],
            vec![DNA("ccGc".to_string())],
        ];
        assert_eq!(actual, should);
    }
}
