#![allow(clippy::unit_arg)]
use itertools::Itertools;
use nom::branch::*;
use nom::bytes::complete::*;
use nom::character::complete::*;
use nom::combinator::value as nom_value;
use nom::combinator::*;
use nom::multi::*;
use nom::number::complete::*;
use nom::sequence::*;
use nom::IResult;
use std::borrow::Cow;
use std::collections::hash_map;
use std::collections::HashMap;
use std::fmt;
use std::fmt::Debug;
use std::fmt::Display;
use std::fmt::Write;

pub trait Insert {
    type V;

    fn insert(self, value: Self::V);
}

impl<'s, K, V> Insert for hash_map::Entry<'s, K, V> {
    type V = V;

    fn insert(self, value: V) {
        match self {
            hash_map::Entry::Occupied(mut entry) => _ = entry.insert(value),
            hash_map::Entry::Vacant(entry) => _ = entry.insert(value),
        }
    }
}

pub type Ident<'code> = &'code str;

#[derive(Clone, Copy)]
pub enum VarIdent<'code> {
    Global(Ident<'code>),
    Local(Ident<'code>),
}

impl<'code> From<VarIdent<'code>> for Ident<'code> {
    fn from(value: VarIdent<'code>) -> Self {
        match value {
            VarIdent::Global(ident) | VarIdent::Local(ident) => ident,
        }
    }
}

impl Display for VarIdent<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Global(ident) => write!(f, "@{}", ident),
            Self::Local(ident) => write!(f, "{}", ident),
        }
    }
}

impl Debug for VarIdent<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        Display::fmt(self, f)
    }
}

#[derive(Clone)]
pub enum ValueSource<'code> {
    Ident(VarIdent<'code>),
    Literal(Value<'code>),
}

impl Display for ValueSource<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Ident(ident) => write!(f, "{}", ident),
            Self::Literal(literal) => write!(f, "{}", literal),
        }
    }
}

impl Debug for ValueSource<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        Display::fmt(self, f)
    }
}

#[derive(Clone)]
pub struct FunctionSignature<'code> {
    pub ident: Ident<'code>,
    pub args: Vec<Ident<'code>>,
    pub out: Option<VarIdent<'code>>,
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

#[derive(Debug, Clone, Copy)]
pub enum JumpType {
    Forced,
    If,
    IfNot,
}

#[derive(Clone)]
pub enum Instruction<'code> {
    SetValue {
        value: ValueSource<'code>,
        out: VarIdent<'code>,
    },
    FunctionCall {
        ident: VarIdent<'code>,
        args: Vec<ValueSource<'code>>,
        out: Option<VarIdent<'code>>,
    },
    LabelDefinition {
        ident: Ident<'code>,
    },
    Go {
        type_: JumpType,
        label: Ident<'code>,
    },
    Return,
    FunctionDefinition {
        signature: FunctionSignature<'code>,
        body: Vec<Instruction<'code>>,
    },
}

impl Display for Instruction<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::SetValue { value, out } => write!(f, "{} > {}", value, out),

            Self::FunctionCall { ident, args, out } => {
                write!(f, "{}({}) > {:?}", ident, args.iter().join(" "), out)
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

            Self::FunctionDefinition { signature, body } => {
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

pub fn can_start_ident(chr: char) -> bool {
    matches!(chr, 'a'..='z' | '-')
}

pub fn is_ident_chr(chr: char) -> bool {
    can_start_ident(chr) || chr.is_ascii_digit()
}

macro_rules! skip_spaces {
    ($input:ident) => {
        let ($input, _) = multispace0($input)?;
    };
}

pub fn wtag<'code, 'r>(
    tag_str: &'r str,
) -> impl Fn(&'code str) -> IResult<&'code str, &'code str> + 'r {
    move |input| {
        skip_spaces!(input);
        tag(tag_str)(input)
    }
}

pub fn keyword<'code: 'r, 'r>(
    keyword: &'r str,
) -> impl FnMut(&'code str) -> IResult<&'code str, &'code str> + 'r {
    verify(ident, move |ident: &str| ident == keyword)
}

pub fn ident(input: &str) -> IResult<&str, Ident> {
    pub fn ident_chrs(input: &str) -> IResult<&str, &str> {
        take_while(is_ident_chr)(input)
    }

    fn starts_as_ident(input: &str) -> bool {
        input.chars().next().map(can_start_ident).unwrap_or_default()
    }

    skip_spaces!(input);
    verify(ident_chrs, starts_as_ident)(input)
}

pub fn idents(input: &str) -> IResult<&str, Vec<Ident>> {
    many0(ident)(input)
}

pub fn var_ident(input: &str) -> IResult<&str, VarIdent> {
    skip_spaces!(input);
    let (input, global_prefix) = opt(wtag("@"))(input)?;
    let (input, ident) = ident(input)?;

    let ident_f = match global_prefix {
        Some(_) => VarIdent::Global,
        None => VarIdent::Local,
    };

    Ok((input, ident_f(ident)))
}

pub fn label_ident(input: &str) -> IResult<&str, Ident> {
    ident(input)
}

pub fn var_idents(input: &str) -> IResult<&str, Vec<VarIdent>> {
    many0(var_ident)(input)
}

pub fn value(input: &str) -> IResult<&str, ValueSource> {
    pub fn number(input: &str) -> IResult<&str, f64> {
        skip_spaces!(input);
        double(input)
    }

    pub fn string(input: &str) -> IResult<&str, &str> {
        delimited(wtag("\""), take_until("\""), tag("\""))(input)
    }

    map(var_ident, ValueSource::Ident)(input)
        .or_else(|_| map(number, |number| ValueSource::Literal(Value::Number(number)))(input))
        .or_else(|_| {
            map(string, |string| ValueSource::Literal(Value::String(Cow::Borrowed(string))))(input)
        })
}

pub fn values(input: &str) -> IResult<&str, Vec<ValueSource>> {
    many0(value)(input)
}

pub fn instruction(input: &str) -> IResult<&str, Instruction> {
    fn out(input: &str) -> IResult<&str, VarIdent> {
        let (input, (_, out)) = (wtag(">"), var_ident).parse(input)?;

        Ok((input, out))
    }

    fn set_value(input: &str) -> IResult<&str, Instruction> {
        let (input, (value, out)) = (value, out).parse(input)?;

        Ok((input, Instruction::SetValue { value, out }))
    }

    fn function_call(input: &str) -> IResult<&str, Instruction> {
        let (input, (ident, _, args, _, out)) =
            (var_ident, wtag("("), values, wtag(")"), opt(out)).parse(input)?;

        Ok((input, Instruction::FunctionCall { ident, args, out }))
    }

    fn label_definition(input: &str) -> IResult<&str, Instruction> {
        let (input, (_, ident)) = (wtag(":"), cut(label_ident)).parse(input)?;

        Ok((input, Instruction::LabelDefinition { ident }))
    }

    fn go(input: &str) -> IResult<&str, Instruction> {
        let (input, (_, label)) = (keyword("go"), cut(label_ident)).parse(input)?;

        Ok((input, Instruction::Go { type_: JumpType::Forced, label }))
    }

    fn goif(input: &str) -> IResult<&str, Instruction> {
        let (input, (_, label)) = (keyword("goif"), cut(label_ident)).parse(input)?;

        Ok((input, Instruction::Go { type_: JumpType::If, label }))
    }

    fn goifn(input: &str) -> IResult<&str, Instruction> {
        let (input, (_, label)) = (keyword("goifn"), cut(label_ident)).parse(input)?;

        Ok((input, Instruction::Go { type_: JumpType::IfNot, label }))
    }

    fn ret(input: &str) -> IResult<&str, Instruction> {
        nom_value(Instruction::Return, keyword("ret"))(input)
    }

    fn function_definition(input: &str) -> IResult<&str, Instruction> {
        fn function_signature(input: &str) -> IResult<&str, FunctionSignature> {
            fn function_signature_args(input: &str) -> IResult<&str, Vec<Ident>> {
                delimited(wtag("("), idents, wtag(")"))(input)
            }

            let (input, (ident, args, out)) =
                (ident, function_signature_args, opt(out)).parse(input)?;

            Ok((input, FunctionSignature { ident, args, out }))
        }

        let (input, (signature, body)) = delimited(
            wtag("{"),
            tuple((cut(function_signature), cut(instructions))),
            cut(wtag("}")),
        )(input)?;

        Ok((input, Instruction::FunctionDefinition { signature, body }))
    }

    let (input, instruction) =
        (set_value, function_call, label_definition, go, goif, goifn, ret, function_definition)
            .choice(input)?;
    Ok((input, instruction))
}

pub fn instructions(input: &str) -> IResult<&str, Vec<Instruction>> {
    let (input, instructions) = many0(instruction)(input)?;

    Ok((input, instructions))
}

#[derive(Debug, Clone)]
pub enum Value<'code> {
    Number(f64),
    String(Cow<'code, str>),
    None,
}

impl Display for Value<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Number(number) => write!(f, "{}", number),
            Self::String(string) => write!(f, "{}", string),
            Self::None => write!(f, "none"),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct CompileError;

#[derive(Debug, Clone, Copy)]
pub struct UnknownVariable;

#[derive(Default)]
pub struct VM<'code> {
    pub globals: HashMap<Ident<'code>, Value<'code>>,
    locals: Vec<HashMap<Ident<'code>, Value<'code>>>,
}

impl<'code> VM<'code> {
    pub fn run(&mut self, code: &'code str) -> Result<(), CompileError> {
        let (_, instructions) = instructions(code).ok().ok_or(CompileError)?;

        Ok(self.run_instructions(&instructions))
    }

    pub fn run_instructions(&mut self, instructions: &[Instruction<'code>]) {
        let mut instruction_idx = 0;

        while instruction_idx < instructions.len() {
            let instruction = &instructions[instruction_idx];
            instruction_idx += 1;

            match *instruction {
                Instruction::SetValue { ref value, out } => {
                    let value = self.value(value).expect("expected variable to exist").clone();

                    match out {
                        VarIdent::Global(ident) => self.globals.insert(ident, value),
                        VarIdent::Local(ident) => self.locals_mut().insert(ident, value),
                    };
                }

                Instruction::FunctionCall { ident, ref args, out } => {
                    let args = args.iter().map(|value| self.value(value).unwrap());

                    let res = match ident.into() {
                        "sum" => {
                            let mut res = 0.;
                            for arg in args {
                                match arg {
                                    Value::Number(number) => res += number,
                                    _ => panic!("expected a number, got '{}'", arg),
                                }
                            }

                            Value::Number(res)
                        }

                        "join" => {
                            let mut res = String::default();
                            for arg in args {
                                write!(&mut res, "{}", arg).unwrap();
                            }

                            Value::String(Cow::Owned(res))
                        }

                        "print" => {
                            for arg in args {
                                print!("{}", arg);
                            }

                            Value::None
                        }

                        "println" => {
                            for arg in args {
                                print!("{}", arg);
                            }
                            println!();

                            Value::None
                        }

                        _ => panic!("function '{}' does not exist", ident),
                    };

                    if let Some(out) = out {
                        self.var_entry(out).insert(res);
                    }
                }

                Instruction::LabelDefinition { .. } => {}

                Instruction::Go { type_: _, label: _ } => todo!(),

                Instruction::Return => break,

                Instruction::FunctionDefinition { .. } => {}
            }
        }
    }

    pub fn locals(&self) -> &HashMap<Ident<'code>, Value<'code>> {
        self.locals.last().unwrap_or(&self.globals)
    }

    pub fn locals_mut(&mut self) -> &mut HashMap<Ident<'code>, Value<'code>> {
        self.locals.last_mut().unwrap_or(&mut self.globals)
    }

    pub fn var(&self, ident: VarIdent<'code>) -> Result<&Value<'code>, UnknownVariable> {
        match ident {
            VarIdent::Global(ident) => self.globals.get(ident),
            VarIdent::Local(ident) => self.locals().get(ident),
        }
        .ok_or(UnknownVariable)
    }

    pub fn var_entry(
        &mut self,
        ident: VarIdent<'code>,
    ) -> hash_map::Entry<&'code str, Value<'code>> {
        match ident {
            VarIdent::Global(ident) => self.globals.entry(ident),
            VarIdent::Local(ident) => self.locals_mut().entry(ident),
        }
    }

    pub fn value<'value>(
        &'value self,
        value: &'value ValueSource<'code>,
    ) -> Result<&'value Value<'code>, UnknownVariable> {
        match *value {
            ValueSource::Ident(ident) => self.var(ident),
            ValueSource::Literal(ref literal) => Ok(literal),
        }
    }
}

fn main() {
    let mut vm = VM::default();

    vm.run(include_str!("code.bar")).unwrap();
}

/*
{ sum-all(list) > sum
    @len(list) > idx
    0 > sum

    :loop
        @eq(idx 0) > @if
        goifn not-zero
            ret
        :not-zero

        @sub(idx 1) > idx
        @get(list idx) > item
        @sum(sum item) > sum
    go loop
}
*/
