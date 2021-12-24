use rust_lox::bytecode::{Compiler, VM};
use rust_lox::frontend::Parser;

use regex::Regex;
use test_generator::test_resources;

#[derive(Debug)]
struct Expected {
    compiled: bool,
    out: Vec<String>,
    runtime_err: Option<String>,
}

#[test_resources("tests/integration/**/*.lox")]
fn test_bytecode_vm(filename: &str) {
    let source = std::fs::read_to_string(filename).unwrap();

    // Get expected and real outputs
    let expected = get_expected_output(&source);
    let actual = run_source_on_vm(&source);

    assert_eq!(expected.compiled, actual.compiled);
    assert_eq!(expected.out, actual.out);
    assert_eq!(expected.runtime_err, actual.runtime_err);
}

fn run_source_on_vm(source: &str) -> Expected {
    // Set up the VM
    let mut out_buf = vec![];
    let mut vm = VM::new_with_output(std::io::Cursor::new(&mut out_buf));

    // Set up the output struct
    let mut result = Expected {
        compiled: false,
        out: vec![],
        runtime_err: None,
    };

    // Parse and compile the source
    let parser = Parser::new(&source);
    let tree = match parser.parse_all() {
        Ok(tree) => tree,
        Err(_) => return result,
    };

    let mut compiler = Compiler::new(vm.borrow_string_table());
    let main_fn = match compiler.compile(&tree.statements) {
        Ok(f) => f,
        Err(_) => return result,
    };
    result.compiled = true;

    // Run the compiled function on the VM
    let vm_result = vm.interpret(main_fn);

    // Set the output appropriately
    result.out = String::from_utf8(out_buf)
        .unwrap()
        .lines()
        .map(|x| x.to_owned())
        .collect();
    result.runtime_err = vm_result.err().map(|e| e.to_string());

    result
}

fn get_expected_output(source: &str) -> Expected {
    let output_regex = Regex::new(r"// expect: (.*)$").unwrap();
    let runtime_err_regex = Regex::new(r"// expect runtime error: (.*)$").unwrap();
    let compile_err_regex = Regex::new(r"// (\[line \d+\] )?(Error.*)").unwrap();

    let mut ex = Expected {
        compiled: true,
        out: vec![],
        runtime_err: None,
    };

    for line in source.lines() {
        if let Some(m) = output_regex.captures(line) {
            ex.out.push(m.get(1).unwrap().as_str().to_owned());
        }
        if let Some(m) = runtime_err_regex.captures(line) {
            ex.runtime_err
                .replace(m.get(1).unwrap().as_str().to_owned());
        }
        if let Some(m) = compile_err_regex.captures(line) {
            ex.compiled = false;
        }
    }

    ex
}
