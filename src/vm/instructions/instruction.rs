use {
    core::fmt,
    std::fmt::{
        Debug,
        Display,
    },
};

mod function_signature;
mod idents;
mod value_source;

pub use {
    function_signature::*,
    idents::*,
    value_source::*,
};

#[derive(Debug, Clone, Copy)]
pub enum JumpType {
    Forced,
    If,
    IfNot,
}

#[derive(Clone)]
pub struct FunctionDefinition<'code> {
    pub signature: FunctionSignature<'code>,
    pub body: Vec<Instruction<'code>>,
}

#[derive(Clone)]
pub enum Instruction<'code> {
    Value {
        value: ValueSource<'code>,
        out: Option<ValueSource<'code>>,
    },
    LabelDefinition {
        ident: Ident<'code>,
    },
    Go {
        type_: JumpType,
        label: Ident<'code>,
    },
    Return,
    FunctionDefinition(FunctionDefinition<'code>),
}

impl Display for Instruction<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Value { value, out } => {
                write!(f, "{}", value)?;

                if let Some(out) = out {
                    write!(f, " > {}", out)?;
                }

                Ok(())
            }

            Self::LabelDefinition { ident } => write!(f, ":{}", ident),

            Self::Go { type_, label } => write!(
                f,
                "{} {}",
                match type_ {
                    JumpType::Forced => "go",
                    JumpType::If => "goif",
                    JumpType::IfNot => "goifn",
                },
                label
            ),

            Self::Return => write!(f, "ret"),

            Self::FunctionDefinition(FunctionDefinition { signature, body }) => {
                write!(f, "{{ {}\n{:#?}\n}}", signature, body)
            }
        }
    }
}

impl Debug for Instruction<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self)
    }
}
