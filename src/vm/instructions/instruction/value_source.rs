use {
    super::VarIdent,
    crate::vm::Value,
    core::fmt,
    std::fmt::{
        Debug,
        Display,
    },
};

#[derive(Clone)]
pub enum RawValueSource<'code> {
    Literal(Value<'code>),
    Variable(VarIdent<'code>),
    FunctionCall {
        ident: VarIdent<'code>,
        args: Vec<ValueSource<'code>>,
    },
}

impl RawValueSource<'_> {
    pub fn is_pure(&self) -> bool {
        match self {
            RawValueSource::Literal(..) => true,
            RawValueSource::Variable(..) => true,
            RawValueSource::FunctionCall { .. } => false,
        }
    }
}

impl Display for RawValueSource<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Self::Literal(ref literal) => write!(f, "{}", literal),

            Self::Variable(ident) => write!(f, "{}", ident),

            Self::FunctionCall { ident, ref args } => {
                write!(f, "{}(", ident)?;

                let mut iter = args.iter();
                if let Some(value) = iter.next() {
                    write!(f, "{}", value)?;

                    for value in iter {
                        write!(f, ", {}", value)?;
                    }
                }

                write!(f, ")")
            }
        }
    }
}

impl Debug for RawValueSource<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        Display::fmt(self, f)
    }
}

#[derive(Clone)]
pub struct ValueSource<'code> {
    pub deref: bool,
    pub value: RawValueSource<'code>,
}

impl Display for ValueSource<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.deref {
            write!(f, "*")?;
        }
        write!(f, "{}", self.value)
    }
}

impl Debug for ValueSource<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        Display::fmt(self, f)
    }
}
