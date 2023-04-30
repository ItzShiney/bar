use itertools::Itertools;
use nom::branch::*;
use nom::bytes::complete::*;
use nom::character::complete::*;
use nom::combinator::*;
use nom::multi::*;
use nom::number::complete::*;
use nom::sequence::*;
use nom::*;
use std::fmt;
use std::fmt::Debug;
use std::fmt::Display;

pub type CommonIdent<'code> = &'code str;

#[derive(Clone, Copy)]
pub enum VarIdent<'code> {
    Global(CommonIdent<'code>),
    Local(CommonIdent<'code>),
}

impl Display for VarIdent<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Global(ident) => write!(f, "@{}", ident),
            Self::Local(ident) => write!(f, "{}", ident),
        }
    }
}

impl Debug for VarIdent<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Display::fmt(self, f)
    }
}

#[derive(Clone, Copy)]
pub struct LabelIdent<'code>(CommonIdent<'code>);

impl Display for LabelIdent<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Debug for LabelIdent<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Display::fmt(self, f)
    }
}

#[derive(Clone, Copy)]
pub struct Number(f64);

impl Display for Number {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Debug for Number {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Display::fmt(self, f)
    }
}

#[derive(Clone, Copy)]
pub enum Value<'code> {
    Ident(VarIdent<'code>),
    Number(Number),
}

impl Display for Value<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Ident(ident) => write!(f, "{}", ident),
            Self::Number(number) => write!(f, "{}", number),
        }
    }
}

impl Debug for Value<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Display::fmt(self, f)
    }
}

pub struct FunctionSignature<'code> {
    pub ident: VarIdent<'code>,
    pub args: Vec<VarIdent<'code>>,
    pub out: Option<VarIdent<'code>>,
}

impl Display for FunctionSignature<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self)
    }
}

#[derive(Debug)]
pub enum JumpType {
    Forced,
    If,
    IfNot,
}

pub enum Instruction<'code> {
    SetValue { value: Value<'code>, out: VarIdent<'code> },
    FunctionCall { ident: VarIdent<'code>, args: Vec<Value<'code>>, out: Option<VarIdent<'code>> },
    LabelDefinition { ident: LabelIdent<'code> },
    Go { type_: JumpType, label: LabelIdent<'code> },
    Return,

    FunctionDefinition { signature: FunctionSignature<'code>, body: Vec<Instruction<'code>> },
}

impl Display for Instruction<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self)
    }
}

pub fn can_start_ident(chr: char) -> bool {
    matches!(chr, 'a'..='z' | '-')
}

pub fn is_ident_chr(chr: char) -> bool {
    can_start_ident(chr) || matches!(chr, '0'..='9')
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
    verify(common_ident, move |ident: &str| ident == keyword)
}

pub fn common_ident(input: &str) -> IResult<&str, CommonIdent> {
    pub fn ident_chrs(input: &str) -> IResult<&str, &str> {
        take_while(is_ident_chr)(input)
    }

    fn starts_as_ident(input: &str) -> bool {
        input.chars().next().map(can_start_ident).unwrap_or_default()
    }

    skip_spaces!(input);
    verify(ident_chrs, starts_as_ident)(input)
}

pub fn var_ident(input: &str) -> IResult<&str, VarIdent> {
    skip_spaces!(input);
    let (input, global_prefix) = opt(wtag("@"))(input)?;
    let (input, ident) = common_ident(input)?;

    let ident_f = match global_prefix {
        Some(_) => VarIdent::Global,
        None => VarIdent::Local,
    };

    Ok((input, ident_f(ident)))
}

pub fn label_ident(input: &str) -> IResult<&str, LabelIdent> {
    map(common_ident, LabelIdent)(input)
}

pub fn idents(input: &str) -> IResult<&str, Vec<VarIdent>> {
    many0(var_ident)(input)
}

pub fn number(input: &str) -> IResult<&str, Number> {
    skip_spaces!(input);
    map(double, Number)(input)
}

pub fn value(input: &str) -> IResult<&str, Value> {
    map(var_ident, Value::Ident)(input).or_else(|_| map(number, Value::Number)(input))
}

pub fn values(input: &str) -> IResult<&str, Vec<Value>> {
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
        let (input, _) = keyword("ret")(input)?;

        Ok((input, Instruction::Return))
    }

    fn function_definition(input: &str) -> IResult<&str, Instruction> {
        fn function_signature(input: &str) -> IResult<&str, FunctionSignature> {
            fn function_signature_args(input: &str) -> IResult<&str, Vec<VarIdent>> {
                delimited(wtag("("), idents, wtag(")"))(input)
            }

            let (input, (ident, args, out)) =
                (var_ident, function_signature_args, opt(out)).parse(input)?;

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

fn main() {
    println!(
        "{:#?}",
        instructions(
            r#"
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
"#
        )
    )
}
