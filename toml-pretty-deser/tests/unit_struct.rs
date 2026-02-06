use toml_pretty_deser::prelude::*;

#[tpd]
#[derive(Debug, PartialEq, Eq)]
struct MyUnit(String);

#[tpd]
#[derive(Debug, PartialEq, Eq)]
struct MyInt(i8);

#[tpd]
#[derive(Debug)]
struct WithUnit {
    a: MyUnit,
    b: MyInt,
}

#[test]
#[allow(clippy::float_cmp)]
fn test_happy_path() {
    let toml = "
        a = 'hello'
         b = 120
        ";

    let result: Result<_, _> = deserialize::<PartialWithUnit, WithUnit>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.a, MyUnit("hello".to_string()));
        assert_eq!(output.b, MyInt(120));
    }
}

#[tpd(partial = false)]
#[derive(Debug)]
struct ReferenceTest {
    segments: Vec<String>,
    #[tpd_default_in_verify]
    inner: SegmentIndex,
}

#[derive(Debug)]
struct SegmentIndex(usize);

impl FromTomlItem for SegmentIndex {
    fn from_toml_item(
        _item: &toml_edit::Item,
        _parent_span: std::ops::Range<usize>,
        _col: &TomlCollector,
    ) -> TomlValue<Self>
    where
        Self: Sized,
    {
        TomlValue {
            value: None,
            state: TomlValueState::NotSet
        }
    }
}

impl VerifyFromToml for PartialReferenceTest {
    fn verify(mut self, helper: &mut TomlHelper<'_>) -> Self {
        //let str_inner: TomlValue<String> = self.inner.get_from_helper(helper); //get with aliases... implement!
        let str_inner: TomlValue<String> = helper.get_with_aliases("inner", &[], true);
        match str_inner.as_ref() {
            Some(segment_name) => {
                if let Some(segments) = self.segments.as_ref() {
                    let index = segments.iter().position(|s| s == segment_name);
                    match index {
                        Some(index) => {
                            self.inner = TomlValue::new_ok(SegmentIndex(index), str_inner.span())
                        }
                        None => {
                            self.inner = TomlValue::new_validation_failed(
                                str_inner.span(),
                                "Not a valid segment".to_string(),
                                Some("use one of the defined ones".to_string()),
                            )
                        }
                    }
                } else {
                    self.inner = TomlValue::new_nested();
                }
            }
            None => {
                self.inner = str_inner.convert_failed_type();
            }
        }

        self
    }
}

#[test]
fn test_segment_happy() {
    let toml = "
        segments = ['a','b']
        inner = 'b'
        ";

    let result: Result<_, _> = deserialize::<PartialReferenceTest, ReferenceTest>(toml);
    dbg!(&result);
    assert!(result.is_ok());
    if let Ok(output) = result {
        assert_eq!(output.inner.0, 1);
        assert_eq!(output.segments.len(), 2);
    }
}
