#![allow(clippy::float_cmp)]
#![allow(clippy::map_unwrap_or)]
#![allow(clippy::match_wildcard_for_single_variants)]
#![allow(clippy::must_use_candidate)]
#![allow(clippy::nonminimal_bool)]
#![allow(clippy::redundant_closure_for_method_calls)]
#![allow(clippy::uninlined_format_args)]

use indexmap::IndexMap;
use toml_pretty_deser::prelude::*;
//library code
//
use toml_pretty_deser::TPDRoot;
use toml_pretty_deser_macros::tpd;

// Manually implemented example on how I want the API to look like
//
// High level description
//
//
// User tag struct T with #[tdp]
// Lib creates
// a) a tpd_from_toml implementation
// b) a PartialT struct with TomlValue<T> of the fields. Note how container types are
//    TomlValue<Container<..., TomlValue<T>>
// c) a VerifyVisitor impl
// d) optionally a blanket <R>VerifyIn<R> impl, if user said #[tdp(no_verify)]
//    Otherwise the user must implement one!
// e) when #[tdp] is applied to enums, they must be either simple enums (then we decode them
//    by their string value +- FieldMatchMode) or tagged single struct containing enums
//    (then they PartialTaggedEnum variants with all the same variants, but inner values replaced
//    with TomlValue<PartialInner> of the inner struct
// f) non type field variations:
//              #[tpd_skip] (ignores the field),
//              #[tpd_default] - set's it's default value iff missing..
//              #[tpd_absorb_remaining] - absorb all remaining values on this key into an IndexMap
//              #[tpd_with=function)] - convert type while deserializing
//
//

#[tpd(root, no_verify)]
#[derive(Debug)]
pub struct Outer {
    #[tpd(alias = "a8", alias = "u8")]
    a_u8: u8,
    opt_u8: Option<u8>,
    vec_u8: Vec<u8>,
    map_u8: IndexMap<String, u8>,
    #[tpd(nested)]
    nested_struct: NestedStruct,
    simple_enum: AnEnum,
    #[tpd(nested)]
    nested_tagged_enum: TaggedEnum,
}

#[tpd]
#[derive(Debug, PartialEq, Eq)]
pub struct NestedStruct {
    other_u8: u8,
    #[tpd(nested)]
    double: DoubleNestedStruct,
}

impl VerifyIn<PartialOuter> for PartialNestedStruct {
    fn verify(&mut self, parent: &PartialOuter) -> Result<(), ValidationFailure> {
        if let Some(value) = self.other_u8.as_mut()
            && let Some(parent_value) = parent.a_u8.value
        {
            *value += parent_value;
        }
        Ok(())
    }
}

#[tpd(no_verify)]
#[derive(Debug, PartialEq, Eq)]
pub struct DoubleNestedStruct {
    double_u8: u8,
}

#[tpd]
#[derive(Debug, PartialEq, Eq)]
pub enum AnEnum {
    TypeA,
    #[tpd(alias = "Bbb", alias = "ccc")]
    TypeB,
    #[tpd(skip)]
    TypeD,
}

#[tpd(tag = "kind", alias = "tag", alias = "type")]
#[derive(Debug, Eq, PartialEq)]
pub enum TaggedEnum {
    KindA(InnerA),
    #[allow(dead_code)]
    #[tpd(alias = "B", alias = "D")]
    KindB(InnerB),
    #[tpd(skip)]
    KindD(InnerA),
}

#[tpd(no_verify)]
#[derive(Debug, Eq, PartialEq)]
pub struct InnerA {
    a: u8,
}

#[tpd(no_verify)]
#[derive(Debug, Eq, PartialEq)]
#[allow(dead_code)]
pub struct InnerB {
    b: u8,
}

#[test]
fn test_basic_happy() {
    let toml = "
        a_u8 = 1
        opt_u8 =2
        vec_u8 = [3]
        simple_enum = 'TypeA'
        [map_u8]
            a = 4
        [nested_struct]
            other_u8 = 5
        [nested_struct.double]
            double_u8 = 6
        [nested_tagged_enum]
            kind = 'KindA'
            a = 100
    ";
    let parsed = Outer::tpd_from_toml(
        toml,
        toml_pretty_deser::FieldMatchMode::Exact,
        toml_pretty_deser::VecMode::Strict,
    );
    dbg!(&parsed);
    assert!(parsed.is_ok());
    if let Ok(inner) = parsed {
        assert_eq!(inner.a_u8, 1);
        assert_eq!(inner.opt_u8, Some(2));
        assert_eq!(inner.vec_u8, vec![3]);
        assert_eq!(inner.simple_enum, AnEnum::TypeA);
        assert_eq!(inner.map_u8.get("a").unwrap(), &4);
        assert_eq!(inner.nested_struct.other_u8, 6); //1 added in verify
        assert_eq!(inner.nested_struct.double.double_u8, 6);
        if let TaggedEnum::KindA(inner) = inner.nested_tagged_enum {
            assert_eq!(inner.a, 100);
        } else {
            panic!("expected KindA variant")
        }
    }
}

#[tpd(root, no_verify)]
#[derive(Debug)]
pub struct OtherOuter {
    #[tpd(nested)]
    pub nested_struct: NestedStruct,
}
impl VerifyIn<PartialOtherOuter> for PartialNestedStruct {
    fn verify(&mut self, _parent: &PartialOtherOuter) -> Result<(), ValidationFailure>
    where
        Self: Sized + toml_pretty_deser::Visitor,
    {
        if let Some(value) = self.other_u8.as_mut() {
            *value += 40;
        }
        Ok(())
    }
}

#[test]
fn test_other_outer() {
    let toml = "
        [nested_struct]
            other_u8 = 5
            double.double_u8 = 6
    ";
    let parsed = OtherOuter::tpd_from_toml(
        toml,
        toml_pretty_deser::FieldMatchMode::Exact,
        toml_pretty_deser::VecMode::Strict,
    );
    dbg!(&parsed);
    assert!(parsed.is_ok());
    if let Ok(inner) = parsed {
        assert_eq!(inner.nested_struct.other_u8, 45); //nothing added in verify.
        assert_eq!(inner.nested_struct.double.double_u8, 6);
    }
}

#[test]
fn test_basic_alias() {
    let toml = "
        u8 = 1
        opt_u8 =2
        vec_u8 = [3]
        simple_enum = 'Bbb'
        [map_u8]
            a = 4
        [nested_struct]
            other_u8 = 5
        [nested_struct.double]
            double_u8 = 6
        [nested_tagged_enum]
            kind = 'KindB'
            b = 200
    ";
    let parsed = Outer::tpd_from_toml(
        toml,
        toml_pretty_deser::FieldMatchMode::Exact,
        toml_pretty_deser::VecMode::Strict,
    );
    dbg!(&parsed);
    assert!(parsed.is_ok());
    if let Ok(inner) = parsed {
        assert_eq!(inner.a_u8, 1);
        assert_eq!(inner.opt_u8, Some(2));
        assert_eq!(inner.vec_u8, vec![3]);
        assert_eq!(inner.simple_enum, AnEnum::TypeB);
        assert_eq!(inner.map_u8.get("a").unwrap(), &4);
        assert_eq!(inner.nested_struct.other_u8, 6); //1 added in verify
        assert_eq!(inner.nested_struct.double.double_u8, 6);
    }
}

#[test]
fn test_basic_alias_anycase() {
    let toml = "
        u_8 = 1
        oPt_u8 =2
        vec-u8 = 3
        SimpleEnum = 'TypeB'
        [MAP--u8]
            a = 4
        [NeStEdStruCt]
            other_u8 = 5
        [NeStEdStruCt.DOUBLE--] # the nested_struct must match
            double_u8 = 6
        [nested_tagged_enum]
            kind = 'KindB'
            b = 200
    ";
    let parsed = Outer::tpd_from_toml(
        toml,
        toml_pretty_deser::FieldMatchMode::AnyCase,
        toml_pretty_deser::VecMode::SingleOk,
    );
    dbg!(&parsed);
    assert!(parsed.is_ok());
    if let Ok(inner) = parsed {
        assert_eq!(inner.a_u8, 1);
        assert_eq!(inner.opt_u8, Some(2));
        assert_eq!(inner.vec_u8, vec![3]);
        assert_eq!(inner.simple_enum, AnEnum::TypeB);
        assert_eq!(inner.map_u8.get("a").unwrap(), &4);
        assert_eq!(inner.nested_struct.other_u8, 6); //1 added in verify
        assert_eq!(inner.nested_struct.double.double_u8, 6);
    }
    let parsed = Outer::tpd_from_toml(
        toml,
        toml_pretty_deser::FieldMatchMode::AnyCase,
        toml_pretty_deser::VecMode::Strict,
    );
    assert!(!parsed.is_ok());
    if let Err(e) = parsed {
        insta::assert_snapshot!(e.pretty("test.toml"));
    } else {
        panic!("expected parsing to fail due to vec mode, but it succeeded");
    }
}

#[test]
fn test_tagged_enum_alias() {
    let toml = "
        u8 = 1
        opt_u8 =2
        vec_u8 = [3]
        simple_enum = 'TypeB'
        [map_u8]
            a = 4
        [nested_struct]
            other_u8 = 5
        [nested_struct.double]
            double_u8 = 6
        [nested_tagged_enum]
            TaG = 'KindB'
            b = 200
    ";
    let parsed = Outer::tpd_from_toml(
        toml,
        toml_pretty_deser::FieldMatchMode::AnyCase,
        toml_pretty_deser::VecMode::Strict,
    );
    dbg!(&parsed);
    assert!(parsed.is_ok());
    if let Ok(inner) = parsed {
        assert_eq!(inner.a_u8, 1);
        assert_eq!(inner.opt_u8, Some(2));
        assert_eq!(inner.vec_u8, vec![3]);
        assert_eq!(inner.simple_enum, AnEnum::TypeB);
        assert_eq!(inner.map_u8.get("a").unwrap(), &4);
        assert_eq!(inner.nested_struct.other_u8, 6); //1 added in verify
        assert_eq!(inner.nested_struct.double.double_u8, 6);
    }
}

#[test]
fn test_basic_missing() {
    let toml = "
        #a_u8 = 1
        opt_u8 =2
        vec_u8 = [3]
        simple_enum = 'TypeA'
        [map_u8]
            a = 4
        [nested_struct]
            other_u8 = 5
        [nested_struct.double]
            double_u8 = 6
    ";
    let parsed = Outer::tpd_from_toml(
        toml,
        toml_pretty_deser::FieldMatchMode::Exact,
        toml_pretty_deser::VecMode::Strict,
    );
    dbg!(&parsed);
    assert!(!parsed.is_ok());
    if let Err(DeserError::DeserFailure(errors, inner)) = parsed {
        assert_eq!(inner.a_u8.value, None);
        assert_eq!(inner.opt_u8.value, Some(Some(2)));
        assert_eq!(inner.vec_u8.value.unwrap()[0].value.unwrap(), 3);
        assert_eq!(inner.simple_enum.value, Some(AnEnum::TypeA));
        insta::assert_snapshot!(errors[0].pretty("test.toml"));
    }
}

#[test]
fn test_basic_unknown() {
    let toml = "
        shu = 23
        a_u8 = 1
        opt_u8 =2
        vec_u8 = [3]
        simple_enum = 'TypeA'
        [map_u8]
            a = 4
        [nested_struct]
            other_u8 = 5
        [nested_struct.double]
            double_u8 = 6
        [nested_tagged_enum]
            a = 10
            kind = 'KindA'
    ";
    let parsed = Outer::tpd_from_toml(
        toml,
        toml_pretty_deser::FieldMatchMode::Exact,
        toml_pretty_deser::VecMode::Strict,
    );
    dbg!(&parsed);
    assert!(!parsed.is_ok());
    if let Err(DeserError::DeserFailure(errors, inner)) = parsed {
        assert_eq!(inner.a_u8.value, Some(1));
        assert_eq!(inner.opt_u8.value, Some(Some(2)));
        assert_eq!(inner.vec_u8.value.unwrap()[0].value.unwrap(), 3);
        assert_eq!(inner.simple_enum.value, Some(AnEnum::TypeA));
        insta::assert_snapshot!(errors[0].pretty("test.toml"));
    }
}
#[test]
fn test_basic_unknown_in_nested() {
    let toml = "
        a_u8 = 1
        opt_u8 =2
        vec_u8 = [3]
        simple_enum = 'TypeA'
        [map_u8]
            a = 4
        [nested_struct]
            shu = 23
            other_u8 = 5
        [nested_struct.double]
            sha = 23
        [nested_tagged_enum]
            a = 10
            kind = 'KindA'
    ";
    let parsed = Outer::tpd_from_toml(
        toml,
        toml_pretty_deser::FieldMatchMode::Exact,
        toml_pretty_deser::VecMode::Strict,
    );
    dbg!(&parsed);
    assert!(!parsed.is_ok());
    if let Err(DeserError::DeserFailure(errors, inner)) = parsed {
        assert_eq!(inner.a_u8.value, Some(1));
        assert_eq!(inner.opt_u8.value, Some(Some(2)));
        assert_eq!(inner.vec_u8.value.as_ref().unwrap()[0].value.unwrap(), 3);
        assert_eq!(inner.simple_enum.value, Some(AnEnum::TypeA));
        assert!(!inner.nested_struct.is_ok());
        insta::assert_snapshot!(DeserError::DeserFailure(errors, inner).pretty("test.toml"));
    }
}

#[test]
fn test_struct_is_no_table() {
    let toml = "
        a_u8 = 1
        opt_u8 =2
        vec_u8 = [3]
        simple_enum = 'TypeA'
        nested_struct = 5
        [map_u8]
            a = 4

        [nested_tagged_enum]
            kind = 'KindA'
            a = 3
    ";
    let parsed = Outer::tpd_from_toml(
        toml,
        toml_pretty_deser::FieldMatchMode::Exact,
        toml_pretty_deser::VecMode::Strict,
    );
    dbg!(&parsed);
    assert!(!parsed.is_ok());
    if let Err(e) = parsed {
        insta::assert_snapshot!(e.pretty("test.toml"));
    }
}
#[test]
fn test_error_in_vec() {
    let toml = "
        a_u8 = 1
        opt_u8 =2
        vec_u8 = ['a']
        simple_enum = 'TypeB'
        [map_u8]
            a = 4
        [nested_struct]
            other_u8 = 5
        [nested_struct.double]
            double_u8 = 6
        [nested_tagged_enum]
            a = 10
            kind = 'KindA'
    ";
    let parsed = Outer::tpd_from_toml(
        toml,
        toml_pretty_deser::FieldMatchMode::Exact,
        toml_pretty_deser::VecMode::Strict,
    );
    dbg!(&parsed);
    assert!(!parsed.is_ok());
    if let Err(DeserError::DeserFailure(errors, inner)) = parsed {
        insta::assert_snapshot!(errors[0].pretty("test.toml"));
        assert_eq!(
            inner
                .map_u8
                .value
                .as_ref()
                .unwrap()
                .map
                .get("a")
                .unwrap()
                .as_ref()
                .unwrap(),
            &4
        );
        assert_eq!(
            inner
                .nested_struct
                .value
                .as_ref()
                .unwrap()
                .other_u8
                .value
                .as_ref(),
            Some(&6)
        ); //1 added in verify
        assert_eq!(
            inner
                .nested_struct
                .value
                .as_ref()
                .unwrap()
                .double
                .as_ref()
                .unwrap()
                .double_u8
                .value
                .as_ref()
                .unwrap(),
            &6
        );
    }
}
#[test]
fn test_error_in_map() {
    let toml = "
        a_u8 = 1
        opt_u8 =2
        vec_u8 = [8]
        simple_enum = 'TypeB'
        [map_u8]
            a = 300
        [nested_struct]
            other_u8 = 5
        [nested_struct.double]
            double_u8 = 6
        [nested_tagged_enum]
            a = 10
            kind = 'KindA'
    ";
    let parsed = Outer::tpd_from_toml(
        toml,
        toml_pretty_deser::FieldMatchMode::Exact,
        toml_pretty_deser::VecMode::Strict,
    );
    dbg!(&parsed);
    assert!(!parsed.is_ok());
    if let Err(e) = parsed {
        insta::assert_snapshot!(e.pretty("test.toml"));
    }
}

#[test]
fn test_error_in_opt() {
    let toml = "
        a_u8 = 1
        opt_u8 ='a'
        vec_u8 = [8]
        simple_enum = 'TypeB'
        [map_u8]
            a = 255
        [nested_struct]
            other_u8 = 5
        [nested_struct.double]
            double_u8 = 6
        [nested_tagged_enum]
            a = 10
            kind = 'KindA'
    ";
    let parsed = Outer::tpd_from_toml(
        toml,
        toml_pretty_deser::FieldMatchMode::Exact,
        toml_pretty_deser::VecMode::Strict,
    );
    dbg!(&parsed);
    assert!(!parsed.is_ok());
    if let Err(e) = parsed {
        insta::assert_snapshot!(e.pretty("test.toml"));
    }
}

#[test]
#[allow(clippy::match_wildcard_for_single_variants)]
fn test_tagged_enum_kind_a() {
    let toml = "
        a_u8 = 1
        opt_u8 = 2
        vec_u8 = [3]
        simple_enum = 'TypeA'
        [map_u8]
            a = 4
        [nested_struct]
            other_u8 = 5
        [nested_struct.double]
            double_u8 = 6
        [nested_tagged_enum]
            kind = 'KindA'
            a = 10
    ";
    let parsed = Outer::tpd_from_toml(
        toml,
        toml_pretty_deser::FieldMatchMode::Exact,
        toml_pretty_deser::VecMode::Strict,
    );
    dbg!(&parsed);
    assert!(parsed.is_ok());
    if let Ok(inner) = parsed {
        assert_eq!(inner.a_u8, 1);
        match inner.nested_tagged_enum {
            TaggedEnum::KindA(ref a) => {
                assert_eq!(a.a, 10);
            }
            _ => panic!("Expected KindA variant"),
        }
    }
}
#[test]
fn test_tagged_enum_kind_b() {
    let toml = "
        a_u8 = 1
        opt_u8 = 2
        vec_u8 = [3]
        simple_enum = 'TypeA'
        [map_u8]
            a = 4
        [nested_struct]
            other_u8 = 5
        [nested_struct.double]
            double_u8 = 6
        [nested_tagged_enum]
            kind = 'KindB'
            b = 20
    ";
    let parsed = Outer::tpd_from_toml(
        toml,
        toml_pretty_deser::FieldMatchMode::Exact,
        toml_pretty_deser::VecMode::Strict,
    );
    dbg!(&parsed);
    assert!(parsed.is_ok());
    if let Ok(inner) = parsed {
        assert_eq!(inner.a_u8, 1);
        match inner.nested_tagged_enum {
            TaggedEnum::KindB(ref b) => {
                assert_eq!(b.b, 20);
            }
            _ => panic!("Expected KindB variant"),
        }
    }
}

#[test]
fn test_tagged_enum_invalid_kind() {
    let toml = "
        a_u8 = 1
        opt_u8 = 2
        vec_u8 = [3]
        simple_enum = 'TypeA'
        [map_u8]
            a = 4
        [nested_struct]
            other_u8 = 5
        [nested_struct.double]
            double_u8 = 6
        [nested_tagged_enum]
            kind = 'InvalidKind'
            a = 10
    ";
    let parsed = Outer::tpd_from_toml(
        toml,
        toml_pretty_deser::FieldMatchMode::Exact,
        toml_pretty_deser::VecMode::Strict,
    );
    dbg!(&parsed);
    assert!(parsed.is_err());
    if let Err(DeserError::DeserFailure(errors, _partial)) = parsed {
        assert!(!errors.is_empty());
        let error_str = errors[0].pretty("test.toml");
        assert!(error_str.contains("Invalid tag value"));
    }
}

#[test]
fn test_tagged_enum_struct_fail() {
    let toml = "
        a_u8 = 1
        opt_u8 = 2
        vec_u8 = [3]
        simple_enum = 'TypeA'
        [map_u8]
            a = 4
        [nested_struct]
            other_u8 = 5
        [nested_struct.double]
            double_u8 = 6
        [nested_tagged_enum]
            b = 10
            kind = 'KindA'
    ";
    let parsed = Outer::tpd_from_toml(
        toml,
        toml_pretty_deser::FieldMatchMode::Exact,
        toml_pretty_deser::VecMode::Strict,
    );
    dbg!(&parsed);
    assert!(parsed.is_err());
    if let Err(e) = &parsed {
        let pretty = e.pretty("test.toml");
        insta::assert_snapshot!(pretty);
    }
}

#[test]
fn test_skipped_enum_kinds() {
    let toml = "
        a_u8 = 1
        opt_u8 =2
        vec_u8 = [3]
        simple_enum = 'TypeD'
        [map_u8]
            a = 4
        [nested_struct]
            other_u8 = 5
        [nested_struct.double]
            double_u8 = 6
        [nested_tagged_enum]
            kind = 'KindD'
            a = 10
    ";
    let parsed = Outer::tpd_from_toml(
        toml,
        toml_pretty_deser::FieldMatchMode::AnyCase,
        toml_pretty_deser::VecMode::Strict,
    );
    dbg!(&parsed);
    assert!(!parsed.is_ok());
    if let Err(e) = parsed {
        insta::assert_snapshot!(e.pretty("test.toml"));
    }
}

#[test]
fn test_enum_aliases() {
    let toml = "
        a_u8 = 1
        opt_u8 =2
        vec_u8 = [3]
        simple_enum = 'ccc' # which gives us a TypeC
        [map_u8]
            a = 4
        [nested_struct]
            other_u8 = 5
        [nested_struct.double]
            double_u8 = 6
        [nested_tagged_enum]
            kind = 'D' # which gives us a KindB
            b = 10
    ";
    let parsed = Outer::tpd_from_toml(
        toml,
        toml_pretty_deser::FieldMatchMode::AnyCase,
        toml_pretty_deser::VecMode::Strict,
    );
    dbg!(&parsed);
    assert!(parsed.is_ok());
    if let Ok(res) = parsed {
        assert_eq!(res.simple_enum, AnEnum::TypeB);
        assert_eq!(res.nested_tagged_enum, TaggedEnum::KindB(InnerB { b: 10 }));
    }
}
#[tpd(root, no_verify)]
pub struct WithVecOfTaggedEnums {
    #[tpd(nested)]
    items: Vec<TaggedEnum>,
}

#[test]
fn test_vec_of_tagged_enums() {
    // Demonstrate that FromTomlItem works with Vec<TaggedEnum>
    let toml_str = "
        [[items]]
        kind = 'KindA'
        a = 1

        [[items]]
        kind = 'KindB'
        b = 2

        [[items]]
        kind = 'KindA'
        a = 3
    ";

    let parsed =
        WithVecOfTaggedEnums::tpd_from_toml(toml_str, FieldMatchMode::Exact, VecMode::Strict);
    assert!(parsed.is_ok());
    if let Ok(parsed) = parsed {
        assert_eq!(parsed.items.len(), 3);
        assert_eq!(parsed.items[0], TaggedEnum::KindA(InnerA { a: 1 }));
        assert_eq!(parsed.items[1], TaggedEnum::KindB(InnerB { b: 2 }));
        assert_eq!(parsed.items[2], TaggedEnum::KindA(InnerA { a: 3 }));
    }
}

#[tpd(root, no_verify)]
pub struct WithVecOfStructs {
    #[tpd(nested)]
    items: Vec<NestedStruct>,
}

impl VerifyIn<PartialWithVecOfStructs> for PartialNestedStruct {}

#[test]
fn test_vec_of_nested_structs() {
    let toml_str = "
        [[items]]
        other_u8 = 10
        [items.double]
        double_u8 = 11

        [[items]]
        other_u8 = 20
        [items.double]
        double_u8 = 21

        [[items]]
        other_u8 = 30
        [items.double]
        double_u8 = 31
    ";

    let parsed = WithVecOfStructs::tpd_from_toml(toml_str, FieldMatchMode::Exact, VecMode::Strict);
    assert!(parsed.is_ok());
    if let Ok(parsed) = parsed {
        assert_eq!(parsed.items.len(), 3);
        assert_eq!(
            parsed.items[0],
            NestedStruct {
                other_u8: 10,
                double: DoubleNestedStruct { double_u8: 11 },
            }
        );
        assert_eq!(
            parsed.items[1],
            NestedStruct {
                other_u8: 20,
                double: DoubleNestedStruct { double_u8: 21 },
            }
        );
        assert_eq!(
            parsed.items[2],
            NestedStruct {
                other_u8: 30,
                double: DoubleNestedStruct { double_u8: 31 },
            }
        );
    }
}

#[tpd(root, no_verify)]
#[derive(Debug)]
pub struct OptionNested {
    #[tpd(nested)]
    a_struct: Option<NestedStruct>,
    #[tpd(nested)]
    tag: Option<TaggedEnum>,
    #[tpd(nested)]
    structs: Option<Vec<NestedStruct>>,
    #[tpd(nested)]
    tagged: Option<Vec<TaggedEnum>>,
}

impl VerifyIn<PartialOptionNested> for PartialNestedStruct {}

#[test]
fn test_options() {
    // Demonstrate that FromTomlItem works with Vec<NestedStruct>
    let toml_str = "
        [a_struct]
            other_u8 = 10
        [a_struct.double]
            double_u8 = 11

        [[structs]]
            other_u8 = 11
            double.double_u8 = 12
        [[structs]]
            other_u8 = 13
            double.double_u8 = 14

        [tag]
            kind = 'KindA'
            a = 15
        [[tagged]]
            kind = 'KindA'
            a = 16

        [[tagged]]
            kind = 'KindB'
            b = 17
    ";

    let parsed = OptionNested::tpd_from_toml(toml_str, FieldMatchMode::Exact, VecMode::Strict);
    dbg!(&parsed);
    assert!(parsed.is_ok());
    if let Ok(parsed) = parsed {
        assert_eq!(parsed.a_struct.as_ref().unwrap().other_u8, 10);
        assert_eq!(parsed.a_struct.as_ref().unwrap().double.double_u8, 11);
        assert_eq!(parsed.structs.as_ref().unwrap().len(), 2);
        assert_eq!(parsed.structs.as_ref().unwrap()[0].other_u8, 11);
        assert_eq!(parsed.structs.as_ref().unwrap()[0].double.double_u8, 12);
        assert_eq!(parsed.structs.as_ref().unwrap().len(), 2);
        assert_eq!(
            parsed.tag.as_ref().unwrap(),
            &TaggedEnum::KindA(InnerA { a: 15 })
        );
        assert_eq!(parsed.tagged.as_ref().unwrap().len(), 2);
        assert_eq!(
            parsed.tagged.as_ref().unwrap()[0],
            TaggedEnum::KindA(InnerA { a: 16 })
        );
        assert_eq!(
            parsed.tagged.as_ref().unwrap()[1],
            TaggedEnum::KindB(InnerB { b: 17 })
        );
    }
}

#[test]
fn test_options_left_off() {
    // Demonstrate that FromTomlItem works with Vec<NestedStruct>
    let toml_str = "
    ";

    let parsed = OptionNested::tpd_from_toml(toml_str, FieldMatchMode::Exact, VecMode::Strict);
    dbg!(&parsed);
    assert!(parsed.is_ok());
    if let Ok(parsed) = parsed {
        assert!(parsed.a_struct.is_none());
        assert!(parsed.tag.is_none());
        assert!(parsed.structs.is_none());
        assert!(parsed.tagged.is_none());
    }
}
#[test]
fn test_options_fail() {
    // Demonstrate that FromTomlItem works with Vec<NestedStruct>
    let toml_str = "
        [a_struct]
            other_u8 = 299
        [a_struct.double]
            double_u8 = 11

        [[structs]]
            other_u8 = 'a'
            double.double_u8 = 12
        [[structs]]
            other_u8 = 13
            double.double_u8 = 14

        [tag]
            kind = 'KindA'
            a = 1500
        [[tagged]]
            kind = 'KindA'
            a = 1600

        [[tagged]]
            kind = 'KindB'
            b = 1700
    ";

    let parsed = OptionNested::tpd_from_toml(toml_str, FieldMatchMode::Exact, VecMode::Strict);
    dbg!(&parsed);

    assert!(!parsed.is_ok());
    if let Err(e) = parsed {
        let pretty = e.pretty("test.toml");
        insta::assert_snapshot!(pretty);
    }
}
#[tpd(root, no_verify)]
#[derive(Debug)]
pub struct MapTest {
    #[tpd(nested)]
    map_nested: IndexMap<String, NestedStruct>,
    #[tpd(nested)]
    map_tagged: IndexMap<String, TaggedEnum>,
    #[tpd(nested)]
    opt_map_nested: Option<IndexMap<String, NestedStruct>>,
    #[tpd(nested)]
    opt_map_tagged: Option<IndexMap<String, TaggedEnum>>,

    #[tpd(nested)]
    map_nested_vec: IndexMap<String, Vec<NestedStruct>>,
    #[tpd(nested)]
    map_tagged_vec: IndexMap<String, Vec<TaggedEnum>>,
}

impl VerifyIn<PartialMapTest> for PartialNestedStruct {}

#[test]
fn test_map() {
    let toml_str = "
        [map_nested.a]
            other_u8 = 10
            double.double_u8 = 11

        [map_nested.b]
            other_u8 = 20
            double.double_u8 = 22

        [map_tagged.first]
            kind = 'KindA'
            a = 10

        [opt_map_nested.c]
            other_u8 = 30
            double.double_u8 = 31

        [opt_map_tagged.second]
            kind = 'KindB'
            b = 20

        [[map_nested_vec.x]]
            other_u8 = 40
            double.double_u8 = 41

        [[map_nested_vec.x]]
            other_u8 = 50
            double.double_u8 = 51

        [[map_tagged_vec.y]]
            kind = 'KindA'
            a = 60

        [[map_tagged_vec.y]]
            kind = 'KindB'
            b = 70
    ";
    let parsed = MapTest::tpd_from_toml(toml_str, FieldMatchMode::Exact, VecMode::Strict);
    dbg!(&parsed);
    assert!(parsed.is_ok());
    if let Ok(parsed) = parsed {
        assert_eq!(parsed.map_nested.get("a").unwrap().other_u8, 10);
        assert_eq!(parsed.map_nested.get("a").unwrap().double.double_u8, 11);
        assert_eq!(parsed.map_nested.get("b").unwrap().other_u8, 20);
        assert_eq!(
            parsed.map_tagged.get("first").unwrap(),
            &TaggedEnum::KindA(InnerA { a: 10 })
        );
        assert_eq!(
            parsed
                .opt_map_nested
                .as_ref()
                .unwrap()
                .get("c")
                .unwrap()
                .other_u8,
            30
        );
        assert_eq!(
            parsed
                .opt_map_nested
                .as_ref()
                .unwrap()
                .get("c")
                .unwrap()
                .double
                .double_u8,
            31
        );
        assert_eq!(
            parsed
                .opt_map_tagged
                .as_ref()
                .unwrap()
                .get("second")
                .unwrap(),
            &TaggedEnum::KindB(InnerB { b: 20 })
        );
        assert_eq!(parsed.map_nested_vec.get("x").unwrap().len(), 2);
        assert_eq!(parsed.map_nested_vec.get("x").unwrap()[0].other_u8, 40);
        assert_eq!(
            parsed.map_nested_vec.get("x").unwrap()[0].double.double_u8,
            41
        );
        assert_eq!(parsed.map_nested_vec.get("x").unwrap()[1].other_u8, 50);
        assert_eq!(
            parsed.map_nested_vec.get("x").unwrap()[1].double.double_u8,
            51
        );
        assert_eq!(parsed.map_tagged_vec.get("y").unwrap().len(), 2);
        assert_eq!(
            parsed.map_tagged_vec.get("y").unwrap()[0],
            TaggedEnum::KindA(InnerA { a: 60 })
        );
    }
}
#[test]
fn test_map_nested() {
    let toml_str = "
        [map_nested.a]
            other_u8 = 10
            double.double_u8 = 'a' # wrong type

        [map_nested.b]
            other_u8 = 20
            double.double_u8 = 22

        [map_tagged.first]
            kind = 'KindA'
            a = 10

        [opt_map_nested.c]
            other_u8 = 30
            double.double_u8 = 31

        [opt_map_tagged.second]
            kind = 'KindC' # invalid kind
            b = 20

        [[map_nested_vec.x]]
            other_u8 = 40
            double.double_u8 = 41

        [[map_nested_vec.x]]
            other_u8 = 50
            double.double_u8 = 51

        [[map_tagged_vec.y]]
            kind = 'KindA'
            a = 'x' # wrong type

        [[map_tagged_vec.y]]
            kind = 'KindB'
            b = 70
    ";
    let parsed = MapTest::tpd_from_toml(toml_str, FieldMatchMode::Exact, VecMode::Strict);
    dbg!(&parsed);
    assert!(!parsed.is_ok());
    if let Err(e) = parsed {
        insta::assert_snapshot!(e.pretty("test.toml"));
    }
}

#[tpd(root)]
#[derive(Debug)]
pub struct WithDefaults {
    a: u8,
    b: u8,
    c: u8,
    #[tpd(default)]
    d: u8,

    #[tpd(skip)]
    s: u8,
}

impl VerifyIn<TPDRoot> for PartialWithDefaults {
    fn verify(&mut self, _parent: &TPDRoot) -> Result<(), ValidationFailure>
    where
        Self: Sized + toml_pretty_deser::Visitor,
    {
        self.a.or_default();
        self.b.or_with(|| 55);
        self.c.verify(|x| {
            if x % 2 == 0 {
                Ok(())
            } else {
                Err((
                    "Must be even".to_string(),
                    Some("Like, 2, or four.".to_string()),
                ))
            }
        });
        self.c.or(33);
        self.s = Some(34);
        Ok(())
    }
}

#[test]
fn test_default() {
    let toml_str = "";
    let parsed = WithDefaults::tpd_from_toml(toml_str, FieldMatchMode::Exact, VecMode::Strict);
    if let Ok(parsed) = parsed {
        assert_eq!(parsed.a, 0); // default for u8
        assert_eq!(parsed.b, 55); // custom default
        assert_eq!(parsed.c, 33); // custom default
        assert_eq!(parsed.d, 0); // Default::default
        assert_eq!(parsed.s, 34); // Skipped, but set in default
    } else {
        panic!("Parsing failed: {:?}", parsed.err());
    }
}

#[test]
fn test_verify() {
    let toml_str = "
    c = 12
";
    let parsed = WithDefaults::tpd_from_toml(toml_str, FieldMatchMode::Exact, VecMode::Strict);
    if let Ok(parsed) = parsed {
        assert_eq!(parsed.a, 0); // default for u8
        assert_eq!(parsed.b, 55); // custom default
        assert_eq!(parsed.c, 12); // custom default
    } else {
        panic!("Parsing failed: {:?}", parsed.err());
    }
}

#[test]
fn test_verify_fail() {
    let toml_str = "
    c = 13
";
    let parsed = WithDefaults::tpd_from_toml(toml_str, FieldMatchMode::Exact, VecMode::Strict);
    if let Err(e) = parsed {
        insta::assert_snapshot!(e.pretty("test.toml"));
    } else {
        panic!("Parsing succeeded?");
    }
}

#[tpd(root, no_verify)]
#[derive(Debug)]
pub struct Absorb {
    anton: u8,
    #[tpd(absorb_remaining)]
    remainder: IndexMap<String, u8>,
    zeta: u8,
}

#[test]
fn test_absorb_remaining() {
    let toml_str = "
        zeta = 4
        anton = 1
        others = 2
        more =3
 ";
    let parsed = Absorb::tpd_from_toml(toml_str, FieldMatchMode::Exact, VecMode::Strict);
    if let Ok(parsed) = parsed {
        assert_eq!(parsed.anton, 1);
        assert_eq!(parsed.zeta, 4);
        assert_eq!(parsed.remainder.get("others").unwrap(), &2);
        assert_eq!(parsed.remainder.get("more").unwrap(), &3);
        assert_eq!(parsed.remainder.len(), 2);
    } else {
        panic!("Parsing failed: {:?}", parsed.err());
    }
}

#[test]
fn test_absorb_remaining_none() {
    let toml_str = "
        anton = 1
        zeta = 4
 ";
    let parsed = Absorb::tpd_from_toml(toml_str, FieldMatchMode::Exact, VecMode::Strict);
    if let Ok(parsed) = parsed {
        assert_eq!(parsed.anton, 1);
        assert_eq!(parsed.remainder.len(), 0);
    } else {
        panic!("Parsing failed: {:?}", parsed.err());
    }
}
#[test]
fn test_absorb_remaining_bad() {
    let toml_str = "
        anton = 1
        others = 2
        more ='a'
        zeta = 4
 ";
    let parsed = Absorb::tpd_from_toml(toml_str, FieldMatchMode::Exact, VecMode::Strict);
    if let Err(e) = parsed {
        insta::assert_snapshot!(e.pretty("test.toml"));
    } else {
        panic!("Parsing succeeded?");
    }
}

#[tpd(root, no_verify)]
#[derive(Debug)]
pub struct TypesTest {
    a_i8: i8,
    a_i16: i16,
    a_i32: i32,
    a_i64: i64,
    a_isize: isize,
    a_u8: u8,
    a_u16: u16,
    a_u32: u32,
    a_u64: u64,
    a_usize: usize,
    a_f64: f64,
    a_bool: bool,
    a_string: String,
    a_from_string: MyFromString,
    a_try_from_string: MyTryFromString,
    #[tpd(with = "adapt_to_upper_case")]
    an_adapted_string: String,
    #[tpd(with = "adapt_from_u8")]
    a_string_from_int: String,
}

pub fn adapt_to_upper_case(input: TomlValue<String>) -> TomlValue<String> {
    input.map(|s| s.to_uppercase())
}

pub fn adapt_from_u8(input: TomlValue<u8>) -> TomlValue<String> {
    input.map(|num| num.to_string())
}

#[derive(Debug)]
struct MyFromString(String);

impl From<&str> for MyFromString {
    fn from(value: &str) -> Self {
        MyFromString(value.to_string())
    }
}
impl_visitor_for_from_str!(MyFromString);

#[derive(Debug)]
struct MyTryFromString(String);

impl_visitor_for_try_from_str!(MyTryFromString, "Longer than 5 letters please");

impl TryFrom<&str> for MyTryFromString {
    type Error = String;
    fn try_from(value: &str) -> Result<Self, Self::Error> {
        if value.len() > 5 {
            Ok(MyTryFromString(value.to_string()))
        } else {
            Err("String too short".to_string())
        }
    }
}

#[test]
#[allow(clippy::field_reassign_with_default)]
#[allow(clippy::bool_assert_comparison)]
fn test_types() {
    let toml_str = "
        a_i8 = -12
        a_i16 = -123
        a_i32 = -123456
        a_i64 = -1234567890123
        a_isize = -12345
        a_u8 = 12
        a_u16 = 123
        a_u32 = 123456
        a_u64 = 1234567890123
        a_usize = 12345
        a_f64 = 34.487
        a_bool = true
        a_string = 'hello'
        a_from_string = 'world'
        a_try_from_string = 'longer than 5'
        an_adapted_string = 'of tomorrow'
        a_string_from_int = 23
    ";
    let parsed = TypesTest::tpd_from_toml(toml_str, FieldMatchMode::Exact, VecMode::Strict);
    if let Ok(parsed) = parsed {
        assert_eq!(parsed.a_i8, -12);
        assert_eq!(parsed.a_i16, -123);
        assert_eq!(parsed.a_i32, -123_456);
        assert_eq!(parsed.a_i64, -1_234_567_890_123);
        assert_eq!(parsed.a_isize, -12_345);
        assert_eq!(parsed.a_u8, 12);
        assert_eq!(parsed.a_u16, 123);
        assert_eq!(parsed.a_u32, 123_456);
        assert_eq!(parsed.a_u64, 1_234_567_890_123);
        assert_eq!(parsed.a_f64, 34.487);
        assert_eq!(parsed.a_usize, 12_345);
        assert_eq!(parsed.a_bool, true);
        assert_eq!(parsed.a_string, "hello");
        assert_eq!(parsed.a_from_string.0, "world");
        assert_eq!(parsed.an_adapted_string, "OF TOMORROW");
        assert_eq!(parsed.a_from_string.0, "world");
        assert_eq!(parsed.a_try_from_string.0, "longer than 5");
        assert_eq!(parsed.a_string_from_int, "23");
    } else {
        panic!("Parsing failed: {:?}", parsed.err());
    }
}

//next type test, string, other integer values, bool
#[test]
fn test_tryfrom_failure() {
    let toml_str = "
        a_i8 = -12
        a_i16 = -123
        a_i32 = -123456
        a_i64 = -1234567890123
        a_isize = -12345
        a_u8 = 12
        a_u16 = 123
        a_u32 = 123456
        a_u64 = 1234567890123
        a_f64 = 34.487
        a_usize = 12345
        a_bool = true
        a_string = 'hello'
        a_from_string = 'w'
        an_adapted_string = 'of tomorrow'
        a_try_from_string = 'shrt'
        a_string_from_int = 'no'
    ";
    let parsed = TypesTest::tpd_from_toml(toml_str, FieldMatchMode::Exact, VecMode::Strict);
    if let Err(e) = parsed {
        insta::assert_snapshot!(e.pretty("test.toml"));
    } else {
        panic!("Parsing succeeded?");
    }
}

//box tests

#[tpd]
#[derive(Debug)]
pub struct BoxedInner {
    name: String,
    value: i32,
}

#[tpd(root, no_verify)]
#[derive(Debug)]
pub struct OuterWithBox {
    #[tpd(nested)]
    boxed: Box<BoxedInner>,
    regular_field: String,
}

#[test]
fn test_box_nested_happy() {
    let toml = "
        regular_field = 'hello'
        [boxed]
            name = 'inner_name'
            value = 42
    ";

    let result: Result<_, _> =
        OuterWithBox::tpd_from_toml(toml, FieldMatchMode::Exact, VecMode::Strict);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.regular_field, "hello");
        assert_eq!(output.boxed.name, "inner_name");
        assert_eq!(output.boxed.value, 42);
    }
}

#[test]
fn test_box_nested_inline_table() {
    let toml = "
        regular_field = 'world'
        boxed = { name = 'inline', value = 100 }
    ";

    let result: Result<_, _> =
        OuterWithBox::tpd_from_toml(toml, FieldMatchMode::Exact, VecMode::Strict);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.regular_field, "world");
        assert_eq!(output.boxed.name, "inline");
        assert_eq!(output.boxed.value, 100);
    }
}

#[test]
fn test_box_nested_missing() {
    let toml = "
        regular_field = 'test'
        # boxed is missing
    ";

    let result: Result<_, _> =
        OuterWithBox::tpd_from_toml(toml, FieldMatchMode::Exact, VecMode::Strict);
    dbg!(&result);
    assert!(result.is_err());
    if let Err(e) = result {
        insta::assert_snapshot!(e.pretty("test.toml"));
    }
}

#[test]
fn test_box_nested_inner_field_missing() {
    let toml = "
        regular_field = 'test'
        [boxed]
            name = 'only_name'
            # value is missing
    ";

    let result: Result<_, _> =
        OuterWithBox::tpd_from_toml(toml, FieldMatchMode::Exact, VecMode::Strict);
    dbg!(&result);
    assert!(result.is_err());

    if let Err(e) = result {
        insta::assert_snapshot!(e.pretty("test.toml"));
        if let DeserError::DeserFailure(_errors, partial) = e {
            // Check that we can still access the partial's boxed inner name
            assert_eq!(
                partial.boxed.value.as_ref().unwrap().name.as_ref().unwrap(),
                "only_name"
            );
        }
    }
}

#[test]
fn test_box_nested_wrong_type() {
    let toml = "
        regular_field = 'test'
        [boxed]
            name = 123
            value = 42
    ";

    let result: Result<_, _> =
        OuterWithBox::tpd_from_toml(toml, FieldMatchMode::Exact, VecMode::Strict);
    dbg!(&result);
    assert!(result.is_err());

    if let Err(e) = result {
        insta::assert_snapshot!(e.pretty("test.toml"));
    }
}

#[derive(Debug)]
struct FailString(String);

toml_pretty_deser::impl_visitor!(FailString, false, |helper| {
    match helper.item.as_str() {
        Some(v) => TomlValue::new_ok(FailString(v.to_string()), helper.span()),
        None => TomlValue::new_wrong_type(helper.item, helper.span(), "string"),
    }
});

impl VerifyIn<PartialMapTestValidationFailure> for FailString {
    fn verify(&mut self, _parent: &PartialMapTestValidationFailure) -> Result<(), ValidationFailure>
    where
        Self: Sized + toml_pretty_deser::Visitor,
    {
        if self.0.len() <= 5 {
            Err(ValidationFailure::new(
                "Too short!",
                Some("Longer than 5 letters please"),
            ))
        } else {
            Ok(())
        }
    }
}

#[tpd(root, no_verify)]
#[derive(Debug)]
#[allow(dead_code)]
pub struct MapTestValidationFailure {
    inner: IndexMap<String, Vec<FailString>>,
}

#[test]
fn test_map_validation_failure() {
    let toml_str = "
        [inner]
        first = ['short']
        second = ['this is long enough']
    ";
    let parsed =
        MapTestValidationFailure::tpd_from_toml(toml_str, FieldMatchMode::Exact, VecMode::Strict);
    //dbg!(&parsed);
    assert!(parsed.is_err());
    if let Err(e) = parsed {
        insta::assert_snapshot!(e.pretty("test.toml"));
    }
}

// () to register arbitrary casess..
#[tpd(root)]
#[derive(Debug)]
#[allow(dead_code)]
pub struct UnitField {
    add_error: (),
    #[tpd(absorb_remaining)]
    remainder: IndexMap<String, String>,
}

impl VerifyIn<TPDRoot> for PartialUnitField {
    fn verify(&mut self, _parent: &TPDRoot) -> Result<(), ValidationFailure>
    where
        Self: Sized + toml_pretty_deser::Visitor,
    {
        let len = self
            .remainder
            .value
            .as_ref()
            .map(|x| x.map.len())
            .unwrap_or(0);
        if !len.is_multiple_of(2) {
            // this is barely useful since we could just return the Err()
            // and end up with pretty much the same error message.
            // Maybe when you need to return multi span errors,
            // and using TomlValue::new_custom?
            self.add_error = TomlValue::new_validation_failed(
                self.remainder.span(),
                "There must be an even number of fields".to_string(),
                Some(format!("There were {} fields", len)),
            );
            // return Err((
            //     "there must be an even number of fields".to_string(),
            //     Some(format!("There were {} fields", len)),
            // ));
        }
        Ok(())
    }
}

#[test]
fn test_unit_field() {
    let toml = "
        cheesecake = 'yes'
        brownies = 'no'

    ";

    let result: Result<_, _> =
        UnitField::tpd_from_toml(toml, FieldMatchMode::Exact, VecMode::Strict);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.remainder.len(), 2);
    }

    let toml = "
        cheesecake = 'yes'
        brownies = 'no'
        cookies = 'very much'

    ";
    let result: Result<_, _> =
        UnitField::tpd_from_toml(toml, FieldMatchMode::Exact, VecMode::Strict);
    dbg!(&result);
    assert!(!result.is_ok());
    if let Err(e) = result {
        insta::assert_snapshot!(e.pretty("test.toml"));
    }
}

#[tpd(root, no_verify)]
#[derive(Debug)]
#[allow(dead_code)]
pub struct NestedUnitField {
    #[tpd(nested)]
    inner: UnitField,
}

#[test]
fn test_nested_unit_field() {
    let toml = "
        [inner]
        cheesecake = 'yes'
        brownies = 'no'
        cookies = 'very much'

    ";
    let result: Result<_, _> =
        NestedUnitField::tpd_from_toml(toml, FieldMatchMode::Exact, VecMode::Strict);
    dbg!(&result);
    assert!(!result.is_ok());
    if let Err(e) = result {
        insta::assert_snapshot!(e.pretty("test.toml"));
    }
}

impl VerifyIn<PartialNestedUnitField> for PartialUnitField {
    fn verify(&mut self, _parent: &PartialNestedUnitField) -> Result<(), ValidationFailure>
    where
        Self: Sized + toml_pretty_deser::Visitor,
    {
        let len = self
            .remainder
            .value
            .as_ref()
            .map(|x| x.map.len())
            .unwrap_or(0);
        if !len.is_multiple_of(2) {
            return Err(ValidationFailure::new(
                "there must be an even number of fields",
                Some(format!("There were {} fields", len)),
            ));
        }
        Ok(())
    }
}

// ===== Module-qualified tagged enum test =====
mod inner_types {
    use toml_pretty_deser_macros::tpd;

    #[tpd(no_verify)]
    #[derive(Debug, PartialEq, Eq)]
    pub struct ModInnerA {
        pub x: i32,
    }

    #[tpd(no_verify)]
    #[derive(Debug, PartialEq, Eq)]
    pub struct ModInnerB {
        pub y: String,
    }
}

#[tpd(tag = "variant")]
#[derive(Debug, PartialEq, Eq)]
enum ModQualifiedTaggedEnum {
    TypeA(inner_types::ModInnerA),
    TypeB(inner_types::ModInnerB),
}

#[tpd(tag = "variant")]
#[derive(Debug, PartialEq, Eq)]
enum BoxedTaggedEnum {
    TypeA(Box<InnerA>),
    TypeB(Box<inner_types::ModInnerB>),
}

#[tpd(root, no_verify)]
#[derive(Debug, PartialEq, Eq)]
struct OuterModQualified {
    #[tpd(nested)]
    item: ModQualifiedTaggedEnum,
}

#[test]
fn test_module_qualified_tagged_enum() {
    let toml = "
        [item]
            variant = 'TypeA'
            x = 42
    ";
    let result = OuterModQualified::tpd_from_toml(
        toml,
        toml_pretty_deser::FieldMatchMode::Exact,
        toml_pretty_deser::VecMode::Strict,
    );
    dbg!(&result);
    assert!(result.is_ok());
    assert_eq!(
        result.unwrap(),
        OuterModQualified {
            item: ModQualifiedTaggedEnum::TypeA(inner_types::ModInnerA { x: 42 })
        }
    );

    let toml = "
        [item]
            variant = 'TypeB'
            y = 'hello'
    ";
    let result = OuterModQualified::tpd_from_toml(
        toml,
        toml_pretty_deser::FieldMatchMode::Exact,
        toml_pretty_deser::VecMode::Strict,
    );
    dbg!(&result);
    assert!(result.is_ok());
    assert_eq!(
        result.unwrap(),
        OuterModQualified {
            item: ModQualifiedTaggedEnum::TypeB(inner_types::ModInnerB {
                y: "hello".to_string()
            })
        }
    );
}

#[tpd(root, no_verify)]
#[derive(Debug, PartialEq, Eq)]
struct OuterBoxedTagged {
    #[tpd(nested)]
    item: BoxedTaggedEnum,
}

#[test]
fn test_boxed_tagged_enum() {
    let toml = "
        [item]
            variant = 'TypeA'
            a = 10
    ";
    let result = OuterBoxedTagged::tpd_from_toml(
        toml,
        toml_pretty_deser::FieldMatchMode::Exact,
        toml_pretty_deser::VecMode::Strict,
    );
    dbg!(&result);
    assert!(result.is_ok());
    assert_eq!(
        result.unwrap(),
        OuterBoxedTagged {
            item: BoxedTaggedEnum::TypeA(Box::new(InnerA { a: 10 }))
        }
    );

    let toml = "
        [item]
            variant = 'TypeB'
            y = 'world'
    ";
    let result = OuterBoxedTagged::tpd_from_toml(
        toml,
        toml_pretty_deser::FieldMatchMode::Exact,
        toml_pretty_deser::VecMode::Strict,
    );
    dbg!(&result);
    assert!(result.is_ok());
    assert_eq!(
        result.unwrap(),
        OuterBoxedTagged {
            item: BoxedTaggedEnum::TypeB(Box::new(inner_types::ModInnerB {
                y: "world".to_string()
            }))
        }
    );
}

#[derive(Debug)]
#[tpd(root)]
pub struct AdaptInVerify {
    #[tpd(adapt_in_verify)] //default to toml_edit::Item if not set.
    inner: usize,
    #[tpd(adapt_in_verify(String))]
    other: usize,
}

impl VerifyIn<TPDRoot> for PartialAdaptInVerify {
    fn verify(&mut self, _parent: &TPDRoot) -> Result<(), ValidationFailure>
    where
        Self: Sized + toml_pretty_deser::Visitor,
    {
        self.other
            .adapt(|value, span| TomlValue::new_ok(value.len(), span));

        self.inner.adapt(|value, span| match value.as_str() {
            Some(v) => TomlValue::new_ok(v.len(), span),
            None => TomlValue::new_wrong_type(&value, span, "string-to-convert"),
        });

        Ok(())
    }
}

#[test]
fn test_adapt_inner() {
    let result: Result<_, _> = AdaptInVerify::tpd_from_toml(
        "
inner = 'hello'
other = 'Europe'

",
        FieldMatchMode::Exact,
        VecMode::Strict,
    );
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.inner, 5);
        assert_eq!(output.other, 6);
    }
}

#[test]
fn test_adapt_inner_wrong_types() {
    let result: Result<_, _> = AdaptInVerify::tpd_from_toml(
        "
inner = 4
other = 123

",
        FieldMatchMode::Exact,
        VecMode::Strict,
    );
    dbg!(&result);
    assert!(!result.is_ok());

    if let Err(e) = result {
        insta::assert_snapshot!(e.pretty("test.toml"));
    }
}

#[derive(Debug)]
#[tpd(root)]
#[allow(dead_code)]
pub struct MapKeyNotStartsWithA {
    inner: IndexMap<String, u8>,
}

impl VerifyIn<TPDRoot> for PartialMapKeyNotStartsWithA {
    fn verify(&mut self, _parent: &TPDRoot) -> Result<(), ValidationFailure>
    where
        Self: Sized + toml_pretty_deser::Visitor,
    {
        self.inner.verify_keys(|key_string| {
            if key_string.starts_with("A") || key_string.starts_with("a") {
                Err(ValidationFailure::new(
                    "Keys cannot start with 'A'",
                    Some("Help text goes here"),
                ))
            } else {
                Ok(())
            }
        });
        Ok(())
    }
}

#[test]
fn test_map_erroron_key() {
    let toml = "
    [inner]
        ok = 42
        absolutly_not = 43
    ";
    let result: Result<_, _> =
        MapKeyNotStartsWithA::tpd_from_toml(toml, FieldMatchMode::Exact, VecMode::Strict);
    if let Err(e) = result {
        insta::assert_snapshot!(e.pretty("test.toml"));
    } else {
        panic!("Parsing succeeded?");
    }
}
