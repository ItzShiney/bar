#![allow(clippy::unit_arg)]
use enum_as_inner::EnumAsInner;
use itertools::EitherOrBoth;
use itertools::Itertools;
use nom::branch::*;
use nom::bytes::complete::*;
use nom::character::complete::*;
use nom::combinator::value as nom_value;
use nom::combinator::*;
use nom::multi::*;
use nom::number::complete::*;
use nom::sequence::*;
use nom::Finish;
use nom::IResult;
use std::borrow::Cow;
use std::cmp::Ordering;
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

pub fn spaces(input: &str) -> IResult<&str, ()> {
    fn spaces(input: &str) -> IResult<&str, ()> {
        nom_value((), multispace1)(input)
    }

    fn line_comment(input: &str) -> IResult<&str, ()> {
        preceded(tag("//"), cut(nom_value((), not_line_ending)))(input)
    }

    fn block_comment(input: &str) -> IResult<&str, ()> {
        delimited(tag("/*"), cut(nom_value((), take_until("*/"))), tag("*/"))(input)
    }

    nom_value((), many0(alt((spaces, line_comment, block_comment))))(input)
}

macro_rules! skip_spaces {
    ($input:ident) => {
        let ($input, _) = $crate::spaces($input)?;
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
    fn keyword_value(input: &str) -> IResult<&str, ValueSource> {
        match ident(input)? {
            (input, "none") => Ok((input, ValueSource::Literal(Value::None))),
            (input, "true") => Ok((input, ValueSource::Literal(Value::Bool(true)))),
            (input, "false") => Ok((input, ValueSource::Literal(Value::Bool(false)))),
            _ => fail(input),
        }
    }

    fn var(input: &str) -> IResult<&str, ValueSource> {
        map(var_ident, ValueSource::Ident)(input)
    }

    fn number(input: &str) -> IResult<&str, ValueSource> {
        skip_spaces!(input);
        let (input, res) = double(input)?;

        Ok((input, ValueSource::Literal(Value::Number(res))))
    }

    fn string(input: &str) -> IResult<&str, ValueSource> {
        let (input, res) = delimited(wtag("\""), take_until("\""), tag("\""))(input)?;

        Ok((input, ValueSource::Literal(Value::String(Cow::Borrowed(res)))))
    }

    alt((keyword_value, var, number, string))(input)
}

pub fn values(input: &str) -> IResult<&str, Vec<ValueSource>> {
    many0(value)(input)
}

pub fn instruction(input: &str) -> IResult<&str, Instruction> {
    fn set_value(input: &str) -> IResult<&str, Instruction> {
        let (input, (value, out)) = separated_pair(value, wtag(">"), cut(var_ident))(input)?;

        Ok((input, Instruction::SetValue { value, out }))
    }

    fn function_call(input: &str) -> IResult<&str, Instruction> {
        let (input, (ident, args, out)) = (
            var_ident,
            delimited(wtag("("), values, wtag(")")),
            opt(preceded(wtag(">"), cut(var_ident))),
        )
            .parse(input)?;

        Ok((input, Instruction::FunctionCall { ident, args, out }))
    }

    fn label_definition(input: &str) -> IResult<&str, Instruction> {
        let (input, (_, ident)) = (wtag(":"), cut(label_ident)).parse(input)?;

        Ok((input, Instruction::LabelDefinition { ident }))
    }

    fn go(input: &str) -> IResult<&str, Instruction> {
        let (input, label) = preceded(keyword("go"), cut(label_ident))(input)?;

        Ok((input, Instruction::Go { type_: JumpType::Forced, label }))
    }

    fn goif(input: &str) -> IResult<&str, Instruction> {
        let (input, label) = preceded(keyword("goif"), cut(label_ident))(input)?;

        Ok((input, Instruction::Go { type_: JumpType::If, label }))
    }

    fn goifn(input: &str) -> IResult<&str, Instruction> {
        let (input, label) = preceded(keyword("goifn"), cut(label_ident))(input)?;

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
                (ident, function_signature_args, opt(preceded(wtag(">"), ident))).parse(input)?;

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
    delimited(spaces, many0(instruction), spaces)(input)
}

#[derive(Debug, Clone, PartialEq, EnumAsInner)]
pub enum Value<'code> {
    None,
    Bool(bool),
    Number(f64),
    String(Cow<'code, str>),
}

impl Display for Value<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Self::None => write!(f, "none"),
            Self::Bool(bool) => write!(f, "{}", bool),
            Self::Number(number) => write!(f, "{}", number),
            Self::String(ref string) => write!(f, "{}", string),
        }
    }
}

impl PartialOrd for Value<'_> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match self {
            Self::None => match other {
                Self::None => Some(Ordering::Equal),
                _ => None,
            },

            Self::Bool(lhs) => match other {
                Self::Bool(rhs) => lhs.partial_cmp(rhs),
                _ => None,
            },

            Self::Number(lhs) => match other {
                Self::Number(rhs) => lhs.partial_cmp(rhs),
                _ => None,
            },

            Self::String(lhs) => match other {
                Self::String(rhs) => lhs.partial_cmp(rhs),
                _ => None,
            },
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
        let (_, instructions) = instructions(code).finish().unwrap();

        Ok(self.run_instructions(&instructions))
    }

    pub fn run_instructions<'s>(&'s mut self, instructions: &'s [Instruction<'code>]) {
        self.run_instructions_local(instructions, instructions)
    }

    fn run_instructions_local<'s>(
        &'s mut self,
        instructions: &'s [Instruction<'code>],
        global_instructions: &'s [Instruction<'code>],
    ) {
        let mut instruction_idx = 0;

        'run: while instruction_idx < instructions.len() {
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
                    fn find_function_definition<'code, 's>(
                        ident: Ident<'code>,
                        mut instructions: impl Iterator<Item = &'s Instruction<'code>>,
                    ) -> Option<&'s Instruction<'code>> {
                        instructions.find(|&instruction| match *instruction {
                            Instruction::FunctionDefinition {
                                signature: FunctionSignature { ident: definition_ident, .. },
                                ..
                            } if definition_ident == ident => true,
                            _ => false,
                        })
                    }

                    let mut args = args.iter().map(|value| self.value(value).unwrap());

                    let res = {
                        let maybe_function_definition = match ident {
                            VarIdent::Global(ident) => {
                                find_function_definition(ident, global_instructions.iter().rev())
                            }

                            VarIdent::Local(ident) => find_function_definition(
                                ident,
                                instructions.iter().rev().chain(global_instructions.iter().rev()),
                            ),
                        };

                        match maybe_function_definition {
                            Some(function_definition) => {
                                let Instruction::FunctionDefinition { signature, body } = function_definition else { unreachable!() };

                                self.locals.push({
                                    let mut locals =
                                        HashMap::<Ident<'code>, Value<'code>>::default();

                                    for either_or_both in
                                        signature.args.iter().copied().zip_longest(args)
                                    {
                                        match either_or_both {
                                            EitherOrBoth::Both(ident, value) => {
                                                locals.insert(ident, value.clone());
                                            }

                                            _ => panic!("argument counts do not match"),
                                        }
                                    }

                                    locals
                                });

                                self.run_instructions_local(&body, global_instructions);

                                let mut locals = self.locals.pop().unwrap();

                                if let Some(out_ident) = signature.out {
                                    let Some(out_value) = locals.remove(out_ident) else {
                                        panic!("expected local variable '{}' to exist, since it is being returned", out_ident);
                                    };

                                    out_value
                                } else {
                                    Value::None
                                }
                            }

                            None => match ident.into() {
                                "sum" => {
                                    let mut res = 0.;
                                    for arg in args {
                                        res += arg.as_number().expect("expected a number");
                                    }

                                    Value::Number(res)
                                }

                                "sub" => {
                                    let mut res = *args
                                        .next()
                                        .expect("expected an argument")
                                        .as_number()
                                        .expect("expected a number");

                                    for arg in args {
                                        res -= arg.as_number().expect("expected a number");
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

                                "le" => Value::Bool(args.tuple_windows().all(|(a, b)| a <= b)),

                                _ => panic!("function '{}' was not found", ident),
                            },
                        }
                    };

                    if let Some(out) = out {
                        self.var_entry(out).insert(res);
                    }
                }

                Instruction::LabelDefinition { .. } => {}

                Instruction::Go { type_, label } => {
                    match type_ {
                        JumpType::Forced => {}

                        JumpType::If => {
                            if !*self
                                .globals
                                .get("if")
                                .expect("expected global variable 'if' to exist, since it is used by 'goif' statement")
                                .as_bool()
                                .expect("expected '@if' to be a bool")
                            {
                                continue;
                            }
                        }

                        JumpType::IfNot => {
                            if *self
                                .globals
                                .get("if")
                                .expect("expected global variable 'if' to exist, since it is used by 'goifn' statement")
                                .as_bool()
                                .expect("expected '@if' to be a bool")
                            {
                                continue;
                            }
                        }
                    }

                    instruction_idx = 0;
                    while instruction_idx < instructions.len() {
                        let instruction = &instructions[instruction_idx];
                        instruction_idx += 1;

                        match *instruction {
                            Instruction::LabelDefinition { ident: definition_ident }
                                if label == definition_ident =>
                            {
                                continue 'run
                            }

                            _ => {}
                        }
                    }

                    panic!("label '{}' was not found", label);
                }

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
