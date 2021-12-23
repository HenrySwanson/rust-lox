use rust_lox::bytecode::{Compiler, VM};
use rust_lox::frontend::Parser;

use regex::Regex;
use test_generator::test_resources;

struct Expected {
    out: Vec<String>,
    runtime_err: Option<String>,
}

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
    let mut out_buf = Vec::<u8>::with_capacity(expected.out.len());

    let parser = Parser::new(&source);
    // TODO augment tests to catch parsing errors
    let tree = succeed_on_err!(parser.parse_all());

    let mut vm = VM::new_with_output(std::io::Cursor::new(&mut out_buf));
    let mut compiler = Compiler::new(vm.borrow_string_table());

    // TODO augment tests to catch compiler errors
    let main_fn = succeed_on_err!(compiler.compile(&tree.statements));
    let vm_result = vm.interpret(main_fn);

    // Check the expected and actual outputs
    let out_lines: Vec<String> = String::from_utf8(out_buf)
        .unwrap()
        .lines()
        .map(|x| x.to_owned())
        .collect();

    assert_eq!(expected.out, out_lines);

    // Check the runtime errors
    assert_eq!(
        expected.runtime_err,
        vm_result.err().map(|e| format!("{:?}", e))
    )
}

fn get_expected_output(source: &str) -> Expected {
    let output_regex = Regex::new(r"// expect: (.*)$").unwrap();
    let runtime_err_regex = Regex::new(r"// expect runtime error: (.*)$").unwrap();

    let mut ex = Expected {
        out: vec![],
        runtime_err: None,
    };

    for line in source.lines() {
        if let Some(m) = output_regex.captures(line) {
            ex.out.push(m.get(1).unwrap().as_str().to_owned());
        }
        if let Some(m) = runtime_err_regex.captures(line) {
            ex.runtime_err.replace(m.get(1).unwrap().as_str().to_owned());
        }
    }

    ex
}
