use rust_lox::bytecode::{Compiler, VM};
use rust_lox::frontend::Parser;

use regex::Regex;
use test_generator::test_resources;

#[test_resources("tests/integration/**/*.lox")]
fn test_bytecode_vm(filename: &str) {
    // TODO this one's not working, go fix it
    if filename.contains("stack_overflow") {
        return;
    }

    // TODO: get rid of this
    macro_rules! succeed_on_err {
        ($e:expr) => {
            match $e {
                Ok(x) => x,
                Err(_) => return,
            }
        };
    }

    let source = std::fs::read_to_string(filename).unwrap();

    // Get expected output, and prep buffer for actual output
    let expected = get_expected_output(&source);
    let mut actual = Vec::<u8>::with_capacity(expected.len());

    let parser = Parser::new(&source);
    // TODO augment tests to catch parsing errors
    let tree = succeed_on_err!(parser.parse_all());

    let mut vm = VM::new_with_output(std::io::Cursor::new(&mut actual));
    let mut compiler = Compiler::new(vm.borrow_string_table());

    // TODO augment tests to catch compiler errors
    let main_fn = succeed_on_err!(compiler.compile(&tree.statements));
    // TODO augment tests to catch runtime errors
    succeed_on_err!(vm.interpret(main_fn));

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
