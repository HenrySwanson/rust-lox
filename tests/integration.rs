use rust_lox::bytecode::{Compiler, VM};
use rust_lox::frontend::Parser;

use regex::Regex;
use test_generator::test_resources;

#[test_resources("tests/integration/*.lox")]
fn test_bytecode_vm(filename: &str) {
    let source = std::fs::read_to_string(filename).unwrap();

    // Get expected output, and prep buffer for actual output
    let expected = get_expected_output(&source);
    let mut actual = Vec::<u8>::with_capacity(expected.len());

    let parser = Parser::new(&source);
    let tree = parser.parse_all().unwrap();

    let mut vm = VM::new_with_output(std::io::Cursor::new(&mut actual));
    let mut compiler = Compiler::new(vm.borrow_string_table());
    let main_fn = compiler.compile(&tree.statements).unwrap();
    vm.interpret(main_fn).unwrap();

    // Check the expected and actual outputs
    let actual: Vec<String> = String::from_utf8(actual)
        .unwrap()
        .lines()
        .map(|x| x.to_owned())
        .collect();
        
    assert_eq!(expected, actual);
}

fn get_expected_output(source: &str) -> Vec<String> {
    let output_regex = Regex::new(r"// expect: (.*)$").unwrap();

    let mut expected_outputs = vec![];
    for line in source.lines() {
        if let Some(m) = output_regex.captures(line) {
            expected_outputs.push(m.get(1).unwrap().as_str().to_owned());
        }
    }

    expected_outputs
}
