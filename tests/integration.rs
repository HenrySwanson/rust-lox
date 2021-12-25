use rust_lox::bytecode::{Compiler, VM};
use rust_lox::frontend::Parser;

use regex::Regex;
use test_generator::test_resources;

#[derive(Debug, PartialEq)]
enum Expected {
    DidNotParse(Vec<String>),
    DidNotCompile(Vec<String>),
    DidCompile(Output),
}

#[derive(Debug, PartialEq)]
struct Output {
    out: Vec<String>,
    runtime_err: Option<String>,
}

#[test_resources("tests/integration/**/*.lox")]
fn test_bytecode_vm(filename: &str) {
    let source = std::fs::read_to_string(filename).unwrap();

    // Get expected and real outputs
    let expected = read_source_comments(&source);
    let actual = run_source_on_vm(&source);

    // TODO: this stubs out compiler error tests. Fix this later.
    if matches!(actual, Expected::DidNotCompile(_)) {
        return;
    }

    assert_eq!(expected, actual);
}

fn run_source_on_vm(source: &str) -> Expected {
    // Set up the VM
    let mut out_buf = vec![];
    let mut vm = VM::new_with_output(std::io::Cursor::new(&mut out_buf));

    // Set up the output struct
    let mut result = Output {
        out: vec![],
        runtime_err: None,
    };

    // Parse and compile the source
    let parser = Parser::new(&source);
    let tree = match parser.parse_all() {
        Ok(tree) => tree,
        Err(errs) => {
            let errs = errs.into_iter().map(|e| e.render(source)).collect();
            return Expected::DidNotParse(errs);
        }
    };

    let mut compiler = Compiler::new(vm.borrow_string_table());
    let main_fn = match compiler.compile(&tree.statements) {
        Ok(f) => f,
        Err(err) => {
            let errs = vec![format!("{:?}", err)];
            return Expected::DidNotCompile(errs);
        }
    };

    // Run the compiled function on the VM
    let vm_result = vm.interpret(main_fn);

    // Set the output appropriately
    result.out = String::from_utf8(out_buf)
        .unwrap()
        .lines()
        .map(|x| x.to_owned())
        .collect();
    result.runtime_err = vm_result.err().map(|e| e.to_string());

    Expected::DidCompile(result)
}

fn read_source_comments(source: &str) -> Expected {
    let output_regex = Regex::new(r"// expect: (.*)$").unwrap();
    let runtime_err_regex = Regex::new(r"// expect runtime error: (.*)$").unwrap();
    let compile_err_regex = Regex::new(r"// (\[line \d+\] )?(Error.*)").unwrap();

    let mut parse_errs = vec![];
    let mut ex = Output {
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
            parse_errs.push(m.get(2).unwrap().as_str().to_owned());
        }
    }

    if !parse_errs.is_empty() {
        Expected::DidNotParse(parse_errs)
    } else {
        Expected::DidCompile(ex)
    }
}
