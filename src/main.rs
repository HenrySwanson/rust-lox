use crate::bytecode::{Compiler, VM};
use crate::common::ast;
use crate::lexer::Lexer;
use crate::parser::{Parser, Resolver};
use crate::treewalk::Interpreter;

use io::Write;
use std::{env, fs, io, process};

const USE_BYTECODE_INTERPRETER: bool = true;

mod bytecode;
mod common;
mod lexer;
mod parser;
mod treewalk;

type RunResult = Result<(), String>;

fn main() {
    let args: Vec<String> = env::args().collect();

    let interpreter: Box<dyn Runnable> = if USE_BYTECODE_INTERPRETER {
        Box::new(VM::new())
    } else {
        Box::new(Interpreter::new())
    };

    match args.len() {
        1 => run_prompt(interpreter),
        2 => run_file(interpreter, &args[1]),
        _ => {
            eprintln!("Usage: rlox [script]");
            process::exit(64);
        }
    }
}

fn run_prompt(mut interpreter: Box<dyn Runnable>) {
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

        match run(interpreter.as_mut(), &input) {
            Ok(_) => {}
            Err(e) => report_error(&e),
        }
    }
}

fn run_file(mut interpreter: Box<dyn Runnable>, filename: &str) {
    let contents = fs::read_to_string(filename).expect("Something went wrong reading the file");

    match run(interpreter.as_mut(), &contents) {
        Ok(_) => {}
        Err(e) => report_error(&e),
    }
}

fn run(interpreter: &mut dyn Runnable, source: &str) -> RunResult {
    // Lex the string
    let lexer = Lexer::new(source);
    let tokens: Vec<_> = lexer.iter().collect();

    // Print the tokens
    // for t in tokens.iter() {
    //     println!("{:?}", t.token);
    // }

    let tokens: Vec<_> = match tokens.into_iter().collect() {
        Ok(tokens) => tokens,
        Err(e) => return Err(e),
    };

    // Parse the tokens
    let parser = Parser::new(tokens.into_iter());
    let statements: Vec<_> = parser.parse_all();

    // Print the AST
    // for s in statements.iter() {
    //     println!("{:?}", s);
    // }

    let statements: Vec<_> = match statements.into_iter().collect() {
        Ok(statements) => statements,
        Err(e) => return Err(format!("{:?}", e)),
    };

    interpreter.consume_statements(statements)
}

fn report_error(err_msg: &str) {
    eprintln!("An error: {}", err_msg);
}

trait Runnable {
    fn consume_statements(&mut self, stmts: Vec<ast::Stmt>) -> RunResult;
}

impl Runnable for Interpreter {
    fn consume_statements(&mut self, mut stmts: Vec<ast::Stmt>) -> RunResult {
        // Resolve variable references
        let mut resolver = Resolver::new();
        for s in stmts.iter_mut() {
            match resolver.resolve_root(s) {
                Ok(_) => (),
                Err(e) => return Err(format!("{:?}", e)),
            }
        }

        // And evaluate the statements
        match self.eval_statements(stmts) {
            Ok(_) => Ok(()),
            Err(e) => Err(format!("{:?}", e)),
        }
    }
}

impl Runnable for VM {
    fn consume_statements(&mut self, stmts: Vec<ast::Stmt>) -> RunResult {
        // Compile the AST
        let mut compiler = Compiler::new(self);

        let main_fn = match compiler.compile(&stmts) {
            Ok(main_fn) => main_fn,
            Err(e) => return Err(format!("{:?}", e)),
        };

        match self.interpret(main_fn) {
            Ok(_) => Ok(()),
            Err(e) => Err(format!("{:?}", e)),
        }
    }
}
