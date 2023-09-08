use {
    self::instructions::{
        program,
        BarVerboseError,
        Instructions,
        RawValueSource,
        ValueSource,
        VarIdent,
    },
    std::collections::hash_map,
};

mod gc_value;
pub mod instructions;
mod value;

use {
    self::instructions::Ident,
    std::collections::HashMap,
};
pub use {
    gc_value::*,
    value::*,
};

#[derive(Debug, Clone, Copy)]
pub struct CompileError;

#[derive(Debug, Clone, Copy)]
pub struct UnknownVariable;

#[derive(Default)]
pub struct VM<'code> {
    pub globals: HashMap<Ident<'code>, GcValue<'code>>,
    locals: Vec<HashMap<Ident<'code>, GcValue<'code>>>,
}

impl<'code> VM<'code> {
    pub fn run(&mut self, code: &'code str) -> Result<(), BarVerboseError<&'code str>> {
        let instructions = program(code)?;

        let mut instructions = Instructions::new(instructions);
        instructions.run(self);

        Ok(())
    }

    pub fn locals(&self) -> &HashMap<Ident<'code>, GcValue<'code>> {
        self.locals.last().unwrap_or(&self.globals)
    }

    pub fn locals_mut(&mut self) -> &mut HashMap<Ident<'code>, GcValue<'code>> {
        self.locals.last_mut().unwrap_or(&mut self.globals)
    }

    pub fn var(&self, ident: VarIdent<'code>) -> Result<&GcValue<'code>, UnknownVariable> {
        match ident {
            VarIdent::Global(ident) => self.globals.get(ident),
            VarIdent::Local(ident) => self.locals().get(ident),
        }
        .ok_or(UnknownVariable)
    }

    pub fn var_mut(
        &mut self,
        ident: VarIdent<'code>,
    ) -> Result<&mut GcValue<'code>, UnknownVariable> {
        match ident {
            VarIdent::Global(ident) => self.globals.get_mut(ident),
            VarIdent::Local(ident) => self.locals_mut().get_mut(ident),
        }
        .ok_or(UnknownVariable)
    }

    pub fn var_entry(
        &mut self,
        ident: VarIdent<'code>,
    ) -> hash_map::Entry<&'code str, GcValue<'code>> {
        match ident {
            VarIdent::Global(ident) => self.globals.entry(ident),
            VarIdent::Local(ident) => self.locals_mut().entry(ident),
        }
    }

    pub fn raw_value(
        &mut self,
        instructions: &mut Instructions<'code>,
        source: &RawValueSource<'code>,
    ) -> GcValue<'code> {
        match *source {
            RawValueSource::Variable(ident) => self
                .var(ident)
                .expect("expected the variable to exist")
                .clone(),

            RawValueSource::Literal(ref literal) => GcValue::new(literal.clone()),

            RawValueSource::FunctionCall { ident, ref args } => {
                instructions.call_function(self, ident, args.clone())
            }
        }
    }

    pub fn value(
        &mut self,
        instructions: &mut Instructions<'code>,
        source: &ValueSource<'code>,
    ) -> GcValue<'code> {
        let value = self.raw_value(instructions, &source.value);

        if source.deref {
            value.cp()
        } else {
            value
        }
    }
}
