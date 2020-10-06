#[allow(clippy::module_inception)]
mod parser;
mod resolver;

pub use parser::Parser;
pub use resolver::Resolver;
