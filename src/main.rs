use rust_lox::bytecode::{Compiler, VM};
use rust_lox::frontend::Parser;
use rust_lox::treewalk::{Interpreter, Resolver};

use clap::{Parser as ClapParser, Subcommand};
use std::io::{Stdout, Write};
use std::{fs, io};

#[derive(ClapParser)]
#[clap(author, version, about, long_about = None)]
struct Cli {
    /// Optional filename to operate on
    filename: Option<String>,

    #[clap(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    Parse,
    Bytecode,
    Treewalker,
}

enum Runnable {
    Parser,
    Treewalker(Interpreter<Stdout>),
    Bytecode(VM<Stdout>),
}

fn main() {
    let cli = Cli::parse();

    let runnable = match cli.command {
        Commands::Parse => Runnable::Parser,
        Commands::Bytecode => Runnable::Bytecode(VM::new()),
        Commands::Treewalker => Runnable::Treewalker(Interpreter::new()),
    };

    match cli.filename {
        Some(filename) => run_from_file(runnable, &filename),
        None => run_from_prompt(runnable),
    }
}

fn run_from_prompt(mut runnable: Runnable) {
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

        match runnable.consume(&input) {
            Ok(()) => {}
            Err(errors) => {
                for error in errors {
                    eprintln!("{}", error);
                }
            }
        }
    }
}

fn run_from_file(mut runnable: Runnable, filename: &str) {
    let contents = fs::read_to_string(filename).expect("Something went wrong reading the file");

    match runnable.consume(&contents) {
        Ok(()) => {}
        Err(errors) => {
            for error in errors {
                eprintln!("{}", error);
            }
        }
    }
}

impl Runnable {
    fn consume(&mut self, source: &str) -> Result<(), Vec<String>> {
        let parser = Parser::new(source);
        let tree = parser.parse_all().map_err(|errors| {
            errors
                .into_iter()
                .map(|e| e.render(source))
                .collect::<Vec<_>>()
        })?;

        match self {
            Runnable::Parser => {
                println!("{:?}", tree);
            }
            Runnable::Treewalker(interpreter) => {
                // Resolve variable references
                let resolver = Resolver::new();
                let tree = resolver
                    .resolve(tree)
                    .map_err(|e| vec![format!("{:?}", e)])?;

                // And evaluate the statements
                interpreter
                    .eval_statements(&tree.statements)
                    .map_err(|e| vec![format!("{:?}", e)])?;
            }
            Runnable::Bytecode(vm) => {
                // Compile the AST
                let mut compiler = Compiler::new(vm.borrow_string_table());

                let main_fn = compiler
                    .compile(&tree.statements)
                    .map_err(|e| vec![format!("{:?}", e)])?;

                vm.interpret(main_fn)
                    .map_err(|e| vec![format!("{:?}", e)])?;
            }
        }

        Ok(())
    }
}
