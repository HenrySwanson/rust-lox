use rust_lox::bytecode::{Compiler, VM};
use rust_lox::frontend::ast;
use rust_lox::frontend::Parser;
use rust_lox::treewalk::{Interpreter, Resolver};

use std::io::Write;
use std::{env, fs, io, process};

const USE_BYTECODE_INTERPRETER: bool = true;

type RunResult = Result<(), String>;

fn main() {
    let args: Vec<String> = env::args().collect();

    let interpreter: Box<dyn Runnable> = if USE_BYTECODE_INTERPRETER {
        Box::new(VM::new())
    } else {
        Box::new(Interpreter::new())
    };

    match args.len() {
        1 => run_from_prompt(interpreter),
        2 => run_from_file(interpreter, &args[1]),
        _ => {
            eprintln!("Usage: rlox [script]");
            process::exit(64);
        }
    };
}

fn run_from_prompt(mut interpreter: Box<dyn Runnable>) {
    loop {
        let mut input = String::new();
        print!("> ");

        loop {
            io::stdout().flush().unwrap();
            let mut new_line = String::new();
            io::stdin()
                .read_line(&mut new_line)
                .expect("Failed to read line");

            // If we get a whitespace only line, break out
            if new_line.trim().is_empty() {
                break;
            }
            input += &new_line;

            print!("  ");
        }

        run(interpreter.as_mut(), input);
    }
}

fn run_from_file(mut interpreter: Box<dyn Runnable>, filename: &str) {
    let contents = fs::read_to_string(filename).expect("Something went wrong reading the file");
    run(interpreter.as_mut(), contents);
}

fn run(interpreter: &mut dyn Runnable, source: String) {
    // Parse the input

    let parser = Parser::new(&source);
    match parser.parse_all() {
        Ok(tree) => match interpreter.consume(tree) {
            Ok(_) => {}
            Err(e) => eprintln!("{:?}", e),
        },
        Err(errors) => {
            eprintln!("Parse errors:");
            for e in errors {
                eprintln!("{}", e)
            }
        }
    }
}

trait Runnable {
    fn consume(&mut self, tree: ast::Tree) -> RunResult;
}

impl<W: std::io::Write> Runnable for Interpreter<W> {
    fn consume(&mut self, tree: ast::Tree) -> RunResult {
        // Resolve variable references
        let resolver = Resolver::new();
        let tree = match resolver.resolve(tree) {
            Ok(t) => t,
            Err(e) => return Err(format!("{:?}", e)),
        };

        // And evaluate the statements
        match self.eval_statements(tree.statements) {
            Ok(_) => Ok(()),
            Err(e) => Err(format!("{:?}", e)),
        }
    }
}

impl<W: std::io::Write> Runnable for VM<W> {
    fn consume(&mut self, tree: ast::Tree) -> RunResult {
        // Compile the AST
        let mut compiler = Compiler::new(self.borrow_string_table());

        let main_fn = match compiler.compile(&tree.statements) {
            Ok(main_fn) => main_fn,
            Err(e) => return Err(format!("{:?}", e)),
        };

        match self.interpret(main_fn) {
            Ok(_) => Ok(()),
            Err(e) => Err(format!("{:?}", e)),
        }
    }
}
