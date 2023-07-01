#![allow(clippy::unit_arg)]
use enum_as_inner::EnumAsInner;
use itertools::EitherOrBoth;
use itertools::Itertools;
use nom::branch::*;
use nom::bytes::complete::*;
use nom::character::complete::*;
use nom::combinator::value as nom_value;
use nom::combinator::*;
use nom::error::*;
use nom::multi::*;
use nom::number::complete::*;
use nom::sequence::*;
use nom::Finish;
use nom::IResult;
use std::borrow::Cow;
use std::cell::RefCell;
use std::cmp::Ordering;
use std::collections::hash_map;
use std::collections::HashMap;
use std::fmt;
use std::fmt::Debug;
use std::fmt::Display;
use std::fmt::Write;
use std::num::FpCategory;
use std::ptr::NonNull;
use std::rc::Rc;

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
pub enum RawValueSource<'code> {
    Literal(Value<'code>),
    Variable(VarIdent<'code>),
    FunctionCall { ident: VarIdent<'code>, args: Vec<ValueSource<'code>> },
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
    deref: bool,
    value: RawValueSource<'code>,
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
pub struct FunctionDefinition<'code> {
    pub signature: FunctionSignature<'code>,
    pub body: Vec<Instruction<'code>>,
}

#[derive(Clone)]
pub enum Instruction<'code> {
    Value { value: ValueSource<'code>, out: Option<ValueSource<'code>> },
    LabelDefinition { ident: Ident<'code> },
    Go { type_: JumpType, label: Ident<'code> },
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

pub fn can_start_ident(chr: char) -> bool {
    matches!(chr, 'a'..='z' | '-')
}

pub fn is_ident_chr(chr: char) -> bool {
    can_start_ident(chr) || chr.is_ascii_digit()
}

pub type BarVerboseError<I> = VerboseError<I>;
pub type BarResult<I, O> = IResult<I, O, VerboseError<I>>;

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
        let ($input, _) = $crate::spaces($input)?;
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
    recognize(pair(take_while_m_n(1, 1, can_start_ident), take_while(is_ident_chr)))(input)
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

        Ok((input, RawValueSource::Literal(Value::String(Cow::Borrowed(res)))))
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

        Ok((input, Instruction::Value { value, out }))
    }

    fn label_definition(input: &str) -> BarResult<&str, Instruction> {
        let (input, (_, ident)) = (wtag(":"), cut(label_ident)).parse(input)?;

        Ok((input, Instruction::LabelDefinition { ident }))
    }

    fn go(input: &str) -> BarResult<&str, Instruction> {
        let (input, label) = preceded(keyword("go"), cut(label_ident))(input)?;

        Ok((input, Instruction::Go { type_: JumpType::Forced, label }))
    }

    fn goif(input: &str) -> BarResult<&str, Instruction> {
        let (input, label) = preceded(keyword("goif"), cut(label_ident))(input)?;

        Ok((input, Instruction::Go { type_: JumpType::If, label }))
    }

    fn goifn(input: &str) -> BarResult<&str, Instruction> {
        let (input, label) = preceded(keyword("goifn"), cut(label_ident))(input)?;

        Ok((input, Instruction::Go { type_: JumpType::IfNot, label }))
    }

    fn ret(input: &str) -> BarResult<&str, Instruction> {
        nom_value(Instruction::Return, keyword("ret"))(input)
    }

    fn function_definition(input: &str) -> BarResult<&str, Instruction> {
        fn function_signature(input: &str) -> BarResult<&str, FunctionSignature> {
            fn function_signature_args(input: &str) -> BarResult<&str, Vec<Ident>> {
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

        Ok((input, Instruction::FunctionDefinition(FunctionDefinition { signature, body })))
    }

    let (input, instruction) =
        (write_value, label_definition, go, goif, goifn, ret, function_definition).choice(input)?;
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

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Trace;

impl Default for Trace {
    fn default() -> Self {
        println!("[Trace::default]");
        Trace
    }
}

impl Display for Trace {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "trace()")
    }
}

impl Clone for Trace {
    fn clone(&self) -> Self {
        println!("[Trace::clone]");
        Trace
    }
}

impl Drop for Trace {
    fn drop(&mut self) {
        println!("[Trace::drop]");
    }
}

#[derive(Debug, Clone, PartialEq, EnumAsInner)]
pub enum Value<'code> {
    None,
    Bool(bool),
    Number(f64),
    String(Cow<'code, str>),
    Trace(Trace),
    List(Vec<ValueRef<'code>>),
}

impl Value<'_> {
    pub fn as_index(&self) -> Option<usize> {
        let res = self.as_number().copied()?;

        if (res.floor() - res).abs() > 1e-2 {
            return None;
        }

        if !matches!(res.classify(), FpCategory::Normal | FpCategory::Zero) {
            return None;
        }

        let res = res as usize;
        let res = res.checked_sub(1)?;

        Some(res)
    }
}

impl From<()> for Value<'_> {
    fn from(_: ()) -> Self {
        Self::None
    }
}

impl From<bool> for Value<'_> {
    fn from(value: bool) -> Self {
        Self::Bool(value)
    }
}

impl From<f64> for Value<'_> {
    fn from(value: f64) -> Self {
        Self::Number(value)
    }
}

impl From<Ordering> for Value<'_> {
    fn from(value: Ordering) -> Self {
        Self::Number(match value {
            Ordering::Less => -1.,
            Ordering::Equal => -1.,
            Ordering::Greater => -1.,
        })
    }
}

impl<'code> From<Cow<'code, str>> for Value<'code> {
    fn from(value: Cow<'code, str>) -> Self {
        Self::String(value)
    }
}

impl<'code, T: Into<Value<'code>>> From<Option<T>> for Value<'code> {
    fn from(value: Option<T>) -> Self {
        match value {
            Some(value) => value.into(),
            None => Self::None,
        }
    }
}

impl Display for Value<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Self::None => write!(f, "none"),
            Self::Bool(bool) => write!(f, "{}", bool),

            Self::Number(number) => {
                if number.is_nan() {
                    write!(f, "{}", "nan")
                } else {
                    write!(f, "{}", number)
                }
            }

            Self::String(ref string) => write!(f, "{}", string),
            Self::Trace(ref trace) => write!(f, "{}", trace),

            Self::List(ref list) => {
                write!(f, "list(")?;

                let mut iter = list.iter();
                if let Some(value) = iter.next() {
                    write!(f, "{}", value.borrow())?;

                    for value in iter {
                        write!(f, ", {}", value.borrow())?;
                    }
                }

                write!(f, ")")
            }
        }
    }
}

impl PartialOrd for Value<'_> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (Self::None, Self::None) => Some(Ordering::Equal),
            (Self::Bool(lhs), Self::Bool(rhs)) => lhs.partial_cmp(rhs),
            (Self::Number(lhs), Self::Number(rhs)) => lhs.partial_cmp(rhs),
            (Self::String(lhs), Self::String(rhs)) => lhs.partial_cmp(rhs),
            (Self::Trace(_), Self::Trace(_)) => Some(Ordering::Equal),
            (Self::List(lhs), Self::List(rhs)) => lhs.partial_cmp(rhs),

            (
                Self::None
                | Self::Bool(_)
                | Self::Number(_)
                | Self::String(_)
                | Self::List(_)
                | Self::Trace(_),
                _,
            ) => None,
        }
    }
}

pub type ValueRef<'code> = Rc<RefCell<Value<'code>>>;

pub fn value_ref(value: Value<'_>) -> ValueRef {
    Rc::new(value.into())
}

pub fn none_ref<'code>() -> ValueRef<'code> {
    value_ref(Value::None)
}

pub fn maybe_value_ref(value: Option<ValueRef<'_>>) -> ValueRef {
    match value {
        None => none_ref(),
        Some(value) => value,
    }
}

pub fn cp<'code>(value: &ValueRef<'code>) -> ValueRef<'code> {
    value_ref(value.borrow().clone())
}

#[derive(Debug, Clone, Copy)]
pub struct CompileError;

#[derive(Debug, Clone, Copy)]
pub struct UnknownVariable;

pub struct Instructions<'code> {
    global: Box<[Instruction<'code>]>,
    locals: Vec<NonNull<[Instruction<'code>]>>,
}

impl<'code> Instructions<'code> {
    pub fn new(global: Vec<Instruction<'code>>) -> Self {
        let global = global.into_boxed_slice();

        Self { global, locals: Default::default() }
    }

    fn local(&self) -> Option<&[Instruction<'code>]> {
        unsafe { Some(self.locals.last()?.as_ref()) }
    }

    fn current(&self) -> &[Instruction<'code>] {
        self.local().unwrap_or(&self.global)
    }

    fn global_rev(&self) -> impl Iterator<Item = &Instruction<'code>> {
        self.global.iter().rev()
    }

    fn local_rev(&self) -> impl Iterator<Item = &Instruction<'code>> {
        self.local().map(|local| local.iter().rev()).unwrap_or_default()
    }

    fn all_rev(&self) -> impl Iterator<Item = &Instruction<'code>> {
        self.local_rev().chain(self.global_rev())
    }

    pub fn run(&mut self, vm: &mut VM<'code>) {
        let mut instruction_idx = 0;

        'run: while instruction_idx < self.current().len() {
            let instruction = self.current()[instruction_idx].clone();
            instruction_idx += 1;

            match instruction {
                Instruction::Value { value, out } => {
                    let value = vm.value(self, &value);

                    if let Some(out) = out {
                        if out.deref {
                            let out = vm.raw_value(self, &out.value);
                            let value = value.borrow().clone();

                            *out.borrow_mut() = value;
                        } else {
                            match out.value {
                                RawValueSource::Variable(ident) => {
                                    match ident {
                                        VarIdent::Global(ident) => vm.globals.insert(ident, value),
                                        VarIdent::Local(ident) => {
                                            vm.locals_mut().insert(ident, value)
                                        }
                                    };
                                }

                                _ => panic!("expected a variable name"),
                            }
                        }
                    }
                }

                Instruction::LabelDefinition { .. } => {}

                Instruction::Go { type_, label } => {
                    match type_ {
                        JumpType::Forced => {}

                        JumpType::If => {
                            if !*vm
                                .globals
                                .get("if")
                                .expect("expected global variable 'if' to exist, since it is used by 'goif' statement")
                                .borrow()
                                .as_bool()
                                .expect("expected '@if' to be a bool")
                            {
                                continue;
                            }
                        }

                        JumpType::IfNot => {
                            if *vm
                                .globals
                                .get("if")
                                .expect("expected global variable 'if' to exist, since it is used by 'goifn' statement")
                                .borrow()
                                .as_bool()
                                .expect("expected '@if' to be a bool")
                            {
                                continue;
                            }
                        }
                    }

                    instruction_idx = 0;
                    while instruction_idx < self.current().len() {
                        let instruction = &self.current()[instruction_idx];
                        instruction_idx += 1;

                        match *instruction {
                            Instruction::LabelDefinition { ident: definition_ident }
                                if label == definition_ident =>
                            {
                                continue 'run;
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

    fn run_function(
        &mut self,
        vm: &mut VM<'code>,
        locals: HashMap<&'code str, ValueRef<'code>>,
        ident: VarIdent<'code>,
    ) -> HashMap<&'code str, ValueRef<'code>> {
        vm.locals.push(locals);

        let instructions: &[_] = &self.find_function_definition(ident).unwrap().body;

        self.locals.push(NonNull::from(instructions));
        self.run(vm);
        self.locals.pop();

        vm.locals.pop().unwrap()
    }

    fn find_function_definition(
        &self,
        ident: VarIdent<'code>,
    ) -> Option<&FunctionDefinition<'code>> {
        fn helper<'code, 's>(
            ident: Ident<'code>,
            instructions: impl Iterator<Item = &'s Instruction<'code>>,
        ) -> Option<&'s FunctionDefinition<'code>> {
            instructions
                .filter_map(|instruction| match instruction {
                    Instruction::FunctionDefinition(
                        res @ FunctionDefinition {
                            signature: FunctionSignature { ident: definition_ident, .. },
                            ..
                        },
                    ) if *definition_ident == ident => Some(res),
                    _ => None,
                })
                .next()
        }

        match ident {
            VarIdent::Global(ident) => helper(ident, self.global_rev()),
            VarIdent::Local(ident) => helper(ident, self.all_rev()),
        }
    }

    pub fn call_function(
        &mut self,
        vm: &mut VM<'code>,
        ident: VarIdent<'code>,
        args: Vec<ValueSource<'code>>,
    ) -> ValueRef<'code> {
        match self.find_function_definition(ident) {
            None => self.call_native_function(vm, ident.into(), args),

            Some(function_definition) => {
                let signature_args = function_definition.signature.args.clone();
                let signature_out = function_definition.signature.out;

                let locals = {
                    let mut locals = HashMap::default();

                    for either_or_both in signature_args.into_iter().zip_longest(args) {
                        match either_or_both {
                            EitherOrBoth::Both(ident, value) => {
                                let value = vm.value(self, &value);
                                locals.insert(ident, value);
                            }

                            _ => panic!("argument counts do not match"),
                        }
                    }

                    locals
                };

                let mut locals = self.run_function(vm, locals, ident);

                if let Some(out_ident) = signature_out {
                    let Some(out_value) = locals.remove(out_ident) else {
                        panic!("expected local variable '{}' to exist, since it is being returned", out_ident);
                    };

                    out_value
                } else {
                    none_ref()
                }
            }
        }
    }

    pub fn call_native_function(
        &mut self,
        vm: &mut VM<'code>,
        ident: Ident<'code>,
        args: Vec<ValueSource<'code>>,
    ) -> ValueRef<'code> {
        let mut args = args.into_iter().map(|value| vm.value(self, &value));

        fn next_arg<'code>(args: &mut impl Iterator<Item = ValueRef<'code>>) -> ValueRef<'code> {
            args.next().expect("expected an argument")
        }

        fn as_number<'code>(arg: ValueRef<'code>) -> f64 {
            arg.borrow().as_number().copied().expect("expected a number")
        }

        fn as_index<'code>(arg: ValueRef<'code>) -> usize {
            arg.borrow().as_index().expect("expected an index")
        }

        fn assert_empty<'code>(mut args: impl Iterator<Item = ValueRef<'code>>) {
            assert!(args.next().is_none(), "invalid arguments count");
        }

        macro_rules! let_borrow {
            ($ref_value:ident) => {
                let $ref_value = $ref_value.borrow();
                let $ref_value = &*$ref_value;
            };

            (mut $ref_value:ident) => {
                let mut $ref_value = $ref_value.borrow_mut();
                let $ref_value = &mut *$ref_value;
            };
        }

        macro_rules! let_as_list {
            ($ref_value:ident) => {
                let $ref_value = $ref_value.as_list().expect("expected a list");
            };

            (mut $ref_value:ident) => {
                let $ref_value = $ref_value.as_list_mut().expect("expected a list");
            };
        }

        macro_rules! verify {
            (
                $res:expr;
                $check:stmt;
            ) => {{
                let res = $res;
                $check
                res
            }};
        }

        let res: ValueRef<'_> = match ident {
            "cp" => verify!(
                value_ref(next_arg(&mut args).borrow().clone());
                assert_empty(args);
            ),

            "sum" => {
                let mut res = 0.;
                for arg in args {
                    res += as_number(arg);
                }

                value_ref(res.into())
            }

            "sub" => {
                let mut res = as_number(next_arg(&mut args));

                for arg in args {
                    res -= as_number(arg);
                }

                value_ref(res.into())
            }

            "join" => {
                let mut res = String::default();
                for arg in args {
                    write!(&mut res, "{}", arg.borrow()).unwrap();
                }

                value_ref(Value::String(Cow::Owned(res)))
            }

            "prin" => {
                for arg in args {
                    print!("{}", arg.borrow());
                }

                none_ref()
            }

            "print" => {
                for arg in args {
                    print!("{}", arg.borrow());
                }
                println!();

                none_ref()
            }

            "cmp" => {
                let (a, b) = args.collect_tuple().expect("expected exactly 2 arguments");

                value_ref(a.partial_cmp(&b).map(Value::from).into())
            }

            "list" => value_ref(Value::List(args.collect())),

            "at" => {
                let list = next_arg(&mut args);
                let_borrow!(list);
                let list = list.as_list().expect("expected a list");

                let idx = as_index(next_arg(&mut args));

                verify!(
                    list[idx].clone();
                    assert_empty(args);
                )
            }

            "push" => {
                let list = next_arg(&mut args);
                let_borrow!(mut list);
                let_as_list!(mut list);

                value_ref(list.extend(args).into())
            }

            "pop" => {
                let list = next_arg(&mut args);
                let_borrow!(mut list);
                let_as_list!(mut list);

                match list.pop().into() {
                    None => none_ref(),
                    Some(value) => value,
                }
            }

            "pop-at" => {
                let list = next_arg(&mut args);
                let_borrow!(mut list);
                let_as_list!(mut list);

                let idx = as_index(next_arg(&mut args));

                verify!(
                    list.remove(idx).into();
                    assert_empty(args);
                )
            }

            "trace" => verify!(
                value_ref(Value::Trace(Trace::default()));
                assert_empty(args);
            ),

            _ => {
                fn get_cmp<'code>(
                    ident: Ident<'code>,
                ) -> Option<fn(ValueRef<'code>, ValueRef<'code>) -> bool> {
                    Some(match ident {
                        // TODO?: a.partial_cmp(b).expect("cannot compare values of different types")
                        "eq" => |a, b| a == b,
                        "ne" => |a, b| a != b,
                        "lt" => |a, b| a < b,
                        "gt" => |a, b| a > b,
                        "le" => |a, b| a <= b,
                        "ge" => |a, b| a >= b,
                        _ => None?,
                    })
                }

                if let Some(cmp) = get_cmp(ident) {
                    value_ref(Value::Bool(args.tuple_windows().all(|(a, b)| cmp(a, b))))
                } else {
                    panic!("function '{}' was not found", ident);
                }
            }
        };

        res
    }
}

#[derive(Default)]
pub struct VM<'code> {
    pub globals: HashMap<Ident<'code>, ValueRef<'code>>,
    locals: Vec<HashMap<Ident<'code>, ValueRef<'code>>>,
}

impl<'code> VM<'code> {
    pub fn run(&mut self, code: &'code str) -> Result<(), BarVerboseError<&'code str>> {
        let instructions = program(code)?;

        let mut instructions = Instructions::new(instructions);
        instructions.run(self);

        Ok(())
    }

    pub fn locals(&self) -> &HashMap<Ident<'code>, ValueRef<'code>> {
        self.locals.last().unwrap_or(&self.globals)
    }

    pub fn locals_mut(&mut self) -> &mut HashMap<Ident<'code>, ValueRef<'code>> {
        self.locals.last_mut().unwrap_or(&mut self.globals)
    }

    pub fn var(&self, ident: VarIdent<'code>) -> Result<&ValueRef<'code>, UnknownVariable> {
        match ident {
            VarIdent::Global(ident) => self.globals.get(ident),
            VarIdent::Local(ident) => self.locals().get(ident),
        }
        .ok_or(UnknownVariable)
    }

    pub fn var_mut(
        &mut self,
        ident: VarIdent<'code>,
    ) -> Result<&mut ValueRef<'code>, UnknownVariable> {
        match ident {
            VarIdent::Global(ident) => self.globals.get_mut(ident),
            VarIdent::Local(ident) => self.locals_mut().get_mut(ident),
        }
        .ok_or(UnknownVariable)
    }

    pub fn var_entry(
        &mut self,
        ident: VarIdent<'code>,
    ) -> hash_map::Entry<&'code str, ValueRef<'code>> {
        match ident {
            VarIdent::Global(ident) => self.globals.entry(ident),
            VarIdent::Local(ident) => self.locals_mut().entry(ident),
        }
    }

    pub fn raw_value(
        &mut self,
        instructions: &mut Instructions<'code>,
        source: &RawValueSource<'code>,
    ) -> ValueRef<'code> {
        match *source {
            RawValueSource::Variable(ident) => {
                self.var(ident).expect("expected the variable to exist").clone()
            }

            RawValueSource::Literal(ref literal) => value_ref(literal.clone()),

            RawValueSource::FunctionCall { ident, ref args } => {
                instructions.call_function(self, ident, args.clone())
            }
        }
    }

    pub fn value(
        &mut self,
        instructions: &mut Instructions<'code>,
        source: &ValueSource<'code>,
    ) -> ValueRef<'code> {
        let value = self.raw_value(instructions, &source.value);

        if source.deref {
            cp(&value)
        } else {
            value
        }
    }
}

fn main() {
    let mut vm = VM::default();
    let input = include_str!("code.bar");

    if let Err(err) = vm.run(input) {
        println!("{}", convert_error(input, err));
    }
}
