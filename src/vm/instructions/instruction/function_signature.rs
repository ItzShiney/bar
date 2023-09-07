use {
    super::Ident,
    core::fmt,
    itertools::Itertools,
    std::fmt::{
        Debug,
        Display,
    },
};

#[derive(Clone)]
pub struct FunctionSignature<'code> {
    pub ident: Ident<'code>,
    pub args: Vec<Ident<'code>>,
    pub out: Option<Ident<'code>>,
}

impl Display for FunctionSignature<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}(", self.ident)?;

        write!(f, "{}", self.args.iter().join(" "))?;

        write!(f, ")")?;
        if let Some(out) = self.out {
            write!(f, " > {}", out)?;
        }
        Ok(())
    }
}

impl Debug for FunctionSignature<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self)
    }
}
