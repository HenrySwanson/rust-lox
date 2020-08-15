use crate::common::token::SpannedToken;
use crate::lexer::Lexer;
use io::Write;
use std::{env, fs, io, process};

mod common;
mod lexer;

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
    let (tokens, errors): (Vec<_>, Vec<_>) = lexer.iter().partition(|r| r.is_ok());
    let tokens: Vec<SpannedToken> = tokens.into_iter().map(Result::unwrap).collect();
    let _errors: Vec<String> = errors.into_iter().map(Result::unwrap_err).collect();

    // For now, just print the tokens
    for t in tokens.iter() {
        println!("{:?}", t.token);
    }

    Ok(())
}

fn report_error(err_msg: &str) {
    eprintln!("An error: {}", err_msg);
}
