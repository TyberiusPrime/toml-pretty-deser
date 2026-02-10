#[test]
fn comp_failures() {
    let t = trybuild::TestCases::new();
    t.compile_fail("compilation_failure_tests/*.rs");
}
