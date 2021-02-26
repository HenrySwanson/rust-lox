use crate::bytecode::{Compiler, VM};
use crate::frontend::ast;
use crate::frontend::Parser;
use crate::treewalk::{Interpreter, Resolver};

use io::Write;
use std::{env, fs, io, process};

mod bytecode;
mod frontend;
mod treewalk;

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
            let bytes_read = io::stdin()
                .read_line(&mut input)
                .expect("Failed to read line");

            // If we get a blank line,
            if bytes_read <= 1 {
                break;
            }

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

impl Runnable for Interpreter {
    fn consume(&mut self, mut tree: ast::Tree) -> RunResult {
        // Resolve variable references
        let mut resolver = Resolver::new();
        for s in tree.statements.iter_mut() {
            match resolver.resolve_root(s) {
                Ok(_) => (),
                Err(e) => return Err(format!("{:?}", e)),
            }
        }

        // And evaluate the statements
        match self.eval_statements(tree.statements) {
            Ok(_) => Ok(()),
            Err(e) => Err(format!("{:?}", e)),
        }
    }
}

impl Runnable for VM {
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
