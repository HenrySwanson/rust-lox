use crate::lexer::Lexer;
use crate::parser::Parser;
use io::Write;
use std::{env, fs, io, process};

mod common;
mod lexer;
mod parser;

type RunResult = Result<(), String>;

fn main() {
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
    loop {
        let mut input = String::new();

        print!("> ");
        io::stdout().flush().unwrap();
        io::stdin()
            .read_line(&mut input)
            .expect("Failed to read line");

        match run(&input) {
            Ok(_) => {}
            Err(e) => report_error(&e),
        }
    }
}

fn run_file(filename: &str) {
    let contents = fs::read_to_string(filename).expect("Something went wrong reading the file");
    match run(&contents) {
        Ok(_) => {}
        Err(e) => report_error(&e),
    }
}

fn run(source: &str) -> RunResult {
    let lexer = Lexer::new(source);
    let tokens: Result<Vec<_>, _> = lexer.iter().collect();

    let mut parser = Parser::new(tokens?.into_iter());

    // For now, just print the expression
    let expr = match parser.parse_expression() {
        Ok(expr) => expr,
        Err(e) => return Err(format!("{:?}", e)),
    };
    println!("{}", expr.lispy_string());

    Ok(())
}

fn report_error(err_msg: &str) {
    eprintln!("An error: {}", err_msg);
}
