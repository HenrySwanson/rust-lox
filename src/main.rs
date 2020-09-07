use crate::lexer::Lexer;
use crate::parser::{Parser, Resolver};
use crate::treewalk::Interpreter;
use io::Write;
use std::{env, fs, io, process};

mod bytecode;
mod common;
mod lexer;
mod parser;
mod treewalk;

type RunResult = Result<(), String>;

fn main() {
    use bytecode::{Chunk, OpCode, VM};
    let mut chunk = Chunk::new();

    let c1 = chunk.add_constant(12);
    let c2 = chunk.add_constant(34);
    let c3 = chunk.add_constant(2);

    chunk.write_instruction(OpCode::Constant, 123);
    chunk.write_byte(c1, 123);

    chunk.write_instruction(OpCode::Constant, 123);
    chunk.write_byte(c2, 123);

    chunk.write_instruction(OpCode::Add, 123);

    chunk.write_instruction(OpCode::Constant, 123);
    chunk.write_byte(c3, 123);

    chunk.write_instruction(OpCode::Divide, 123);
    chunk.write_instruction(OpCode::Negate, 123);

    chunk.write_instruction(OpCode::Return, 123);

    match VM::new().interpret(&chunk) {
        Ok(_) => (),
        Err(e) => println!("Error: {:?}", e),
    }
}

// TODO restore to main
#[allow(dead_code)]
fn main_() {
    let args: Vec<String> = env::args().collect();

    match args.len() {
        1 => run_prompt(),
        2 => run_file(&args[1]),
        _ => {
            eprintln!("Usage: rlox [script]");
            process::exit(64);
        }
    }
}

fn run_prompt() {
    let mut interpreter = Interpreter::new();

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

        match run(&mut interpreter, &input) {
            Ok(_) => {}
            Err(e) => report_error(&e),
        }
    }
}

fn run_file(filename: &str) {
    let contents = fs::read_to_string(filename).expect("Something went wrong reading the file");
    let mut interpreter = Interpreter::new();

    match run(&mut interpreter, &contents) {
        Ok(_) => {}
        Err(e) => report_error(&e),
    }
}

fn run(interpreter: &mut Interpreter, source: &str) -> RunResult {
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

    let mut statements: Vec<_> = match statements.into_iter().collect() {
        Ok(statements) => statements,
        Err(e) => return Err(format!("{:?}", e)),
    };

    // Resolve variable references
    let mut resolver = Resolver::new();
    for s in statements.iter_mut() {
        match resolver.resolve_root(s) {
            Ok(_) => (),
            Err(e) => return Err(format!("{:?}", e)),
        }
    }

    // And evaluate the statements
    match interpreter.eval_statements(statements) {
        Ok(_) => Ok(()),
        Err(e) => Err(format!("{:?}", e)),
    }
}

fn report_error(err_msg: &str) {
    eprintln!("An error: {}", err_msg);
}
