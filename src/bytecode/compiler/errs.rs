#[derive(Debug)]
pub enum CompilerError {
    LocalAlreadyExists(String),
    LocalUsedInOwnInitializer(String),
    TooManyConstants,
    TooManyLocals,
    TooManyUpvalues,
    JumpTooLong,
    SelfInherit(String),
    ThisOutsideClass,
    SuperOutsideClass,
    SuperWithoutSuperclass,
    ReturnAtTopLevel,
    ReturnInInitializer,
}

pub type CompilerResult<T> = Result<T, CompilerError>;
