use thiserror::Error;

#[derive(Error, Debug, Clone)]
pub enum Error {
    #[error("Parse Error: {0}")]
    ParseError(String),
    #[error("Type Error: (expected {expected:?}, found {found:?})")]
    TypeChecking { expected: String, found: String },
    #[error("Run time Error: {0}")]
    Runtime(String),
}
