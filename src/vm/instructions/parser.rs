use {
    super::{
        Ident,
        Instruction,
        RawValueSource,
        VarIdent,
    },
    crate::vm::{
        instructions::{
            FunctionDefinition,
            FunctionSignature,
            JumpType,
            ValueSource,
        },
        Value,
    },
    nom::{
        branch::*,
        bytes::complete::*,
        character::complete::*,
        combinator::{
            value as nom_value,
            *,
        },
        error::*,
        multi::*,
        number::complete::double,
        sequence::*,
        Finish,
        IResult,
    },
    std::borrow::Cow,
};

pub type BarVerboseError<I> = VerboseError<I>;
pub type BarResult<I, O> = IResult<I, O, VerboseError<I>>;

fn can_start_ident(chr: char) -> bool {
    matches!(chr, 'a'..='z' | '-')
}

fn is_ident_chr(chr: char) -> bool {
    can_start_ident(chr) || chr.is_ascii_digit()
}

pub fn spaces(input: &str) -> BarResult<&str, ()> {
    fn spaces(input: &str) -> BarResult<&str, ()> {
        nom_value((), multispace1)(input)
    }

    fn line_comment(input: &str) -> BarResult<&str, ()> {
        preceded(tag("//"), cut(nom_value((), not_line_ending)))(input)
    }

    fn block_comment(input: &str) -> BarResult<&str, ()> {
        delimited(tag("/*"), cut(nom_value((), take_until("*/"))), tag("*/"))(input)
    }

    nom_value((), many0(alt((spaces, line_comment, block_comment))))(input)
}

macro_rules! skip_spaces {
    ($input:ident) => {
        let ($input, _) = spaces($input)?;
    };
}

pub fn wtag<'code, 'r>(
    tag_str: &'r str,
) -> impl Fn(&'code str) -> BarResult<&'code str, &'code str> + 'r {
    move |input| {
        skip_spaces!(input);
        tag(tag_str)(input)
    }
}

pub fn keyword<'code: 'r, 'r>(
    keyword: &'r str,
) -> impl FnMut(&'code str) -> BarResult<&'code str, &'code str> + 'r {
    verify(ident, move |ident: &str| ident == keyword)
}

pub fn ident(input: &str) -> BarResult<&str, Ident> {
    skip_spaces!(input);
    recognize(pair(
        take_while_m_n(1, 1, can_start_ident),
        take_while(is_ident_chr),
    ))(input)
}

pub fn idents(input: &str) -> BarResult<&str, Vec<Ident>> {
    many0(ident)(input)
}

pub fn var_ident(input: &str) -> BarResult<&str, VarIdent> {
    skip_spaces!(input);
    let (input, global_prefix) = opt(wtag("@"))(input)?;
    let (input, ident) = ident(input)?;

    let ident_f = match global_prefix {
        Some(_) => VarIdent::Global,
        None => VarIdent::Local,
    };

    Ok((input, ident_f(ident)))
}

pub fn label_ident(input: &str) -> BarResult<&str, Ident> {
    ident(input)
}

pub fn var_idents(input: &str) -> BarResult<&str, Vec<VarIdent>> {
    many0(var_ident)(input)
}

pub fn raw_value(input: &str) -> BarResult<&str, RawValueSource> {
    fn keyword_value(input: &str) -> BarResult<&str, RawValueSource> {
        let (new_input, res) = ident(input)?;

        let res = match res {
            "none" => Value::None,
            "true" => Value::Bool(true),
            "false" => Value::Bool(false),
            "nan" => Value::Number(f64::NAN),
            "inf" => Value::Number(f64::INFINITY),
            "-inf" => Value::Number(f64::NEG_INFINITY),
            _ => return fail(input),
        };
        let res = RawValueSource::Literal(res);

        Ok((new_input, res))
    }

    fn var(input: &str) -> BarResult<&str, RawValueSource> {
        fn args(input: &str) -> BarResult<&str, Vec<ValueSource>> {
            delimited(wtag("("), cut(values), cut(wtag(")")))(input)
        }

        let (input, ident) = var_ident(input)?;

        Ok(match args(input) {
            Err(_) => (input, RawValueSource::Variable(ident)),
            Ok((input, args)) => (input, RawValueSource::FunctionCall { ident, args }),
        })
    }

    fn number(input: &str) -> BarResult<&str, RawValueSource> {
        skip_spaces!(input);
        let (input, res) = double(input)?;

        Ok((input, RawValueSource::Literal(Value::Number(res))))
    }

    fn string(input: &str) -> BarResult<&str, RawValueSource> {
        let (input, res) = delimited(wtag("\""), take_until("\""), tag("\""))(input)?;

        Ok((
            input,
            RawValueSource::Literal(Value::String(Cow::Borrowed(res))),
        ))
    }

    alt((number, keyword_value, var, string))(input)
}

pub fn value(input: &str) -> BarResult<&str, ValueSource> {
    let (input, deref) = map(opt(wtag("*")), |res| res.is_some())(input)?;

    map(raw_value, move |value| ValueSource { deref, value })(input)
}

pub fn values(input: &str) -> BarResult<&str, Vec<ValueSource>> {
    many0(value)(input)
}

pub fn instruction(input: &str) -> BarResult<&str, Instruction> {
    fn instruction_out(input: &str) -> BarResult<&str, ValueSource> {
        preceded(wtag(">"), cut(value))(input)
    }

    fn write_value(input: &str) -> BarResult<&str, Instruction> {
        let (input, (value, out)) = pair(value, opt(instruction_out))(input)?;

        if value.value.is_pure() && out.is_none() {
            return fail(input);
        }

        Ok((input, Instruction::Value { value, out }))
    }

    fn label_definition(input: &str) -> BarResult<&str, Instruction> {
        let (input, (_, ident)) = (wtag(":"), cut(label_ident)).parse(input)?;

        Ok((input, Instruction::LabelDefinition { ident }))
    }

    fn go(input: &str) -> BarResult<&str, Instruction> {
        let (input, label) = preceded(keyword("go"), cut(label_ident))(input)?;

        Ok((
            input,
            Instruction::Go {
                type_: JumpType::Forced,
                label,
            },
        ))
    }

    fn goif(input: &str) -> BarResult<&str, Instruction> {
        let (input, label) = preceded(keyword("goif"), cut(label_ident))(input)?;

        Ok((
            input,
            Instruction::Go {
                type_: JumpType::If,
                label,
            },
        ))
    }

    fn goifn(input: &str) -> BarResult<&str, Instruction> {
        let (input, label) = preceded(keyword("goifn"), cut(label_ident))(input)?;

        Ok((
            input,
            Instruction::Go {
                type_: JumpType::IfNot,
                label,
            },
        ))
    }

    fn ret(input: &str) -> BarResult<&str, Instruction> {
        nom_value(Instruction::Return, keyword("ret"))(input)
    }

    fn function_definition(input: &str) -> BarResult<&str, Instruction> {
        fn function_signature(input: &str) -> BarResult<&str, FunctionSignature> {
            fn function_signature_args(input: &str) -> BarResult<&str, Vec<Ident>> {
                delimited(wtag("("), idents, wtag(")"))(input)
            }

            let (input, (ident, args, out)) = (
                ident,
                function_signature_args,
                opt(preceded(wtag(">"), ident)),
            )
                .parse(input)?;

            Ok((input, FunctionSignature { ident, args, out }))
        }

        let (input, (signature, body)) = delimited(
            wtag("{"),
            tuple((cut(function_signature), cut(instructions))),
            cut(wtag("}")),
        )(input)?;

        Ok((
            input,
            Instruction::FunctionDefinition(FunctionDefinition { signature, body }),
        ))
    }

    let (input, instruction) = (
        write_value,
        label_definition,
        go,
        goif,
        goifn,
        ret,
        function_definition,
    )
        .choice(input)?;
    Ok((input, instruction))
}

pub fn instructions(input: &str) -> BarResult<&str, Vec<Instruction>> {
    many0(instruction)(input)
}

pub fn program(input: &str) -> Result<Vec<Instruction>, BarVerboseError<&str>> {
    let (input, res) = instructions(input).finish()?;
    let (input, _) = spaces(input).finish()?;
    eof(input).finish()?;

    Ok(res)
}
