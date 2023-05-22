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
use std::marker::PhantomPinned;
use std::pin::Pin;
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

#[derive(Debug, Clone, Copy)]
pub struct Derefs<'code> {
    pub ident: VarIdent<'code>,
    pub times: usize,
}

#[derive(Clone)]
pub enum ValueSource<'code> {
    Variable(VarIdent<'code>),
    Literal(Value<'code>),
    TakeRef(VarIdent<'code>),
    Derefs(Derefs<'code>),
    FunctionCall { ident: VarIdent<'code>, args: Vec<Self> },
}

impl Display for ValueSource<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Self::Variable(ident) => write!(f, "{}", ident),
            Self::Literal(ref literal) => write!(f, "{}", literal),
            Self::TakeRef(ident) => write!(f, "?{}", ident),
            Self::Derefs(Derefs { ident, times }) => {
                write!(f, "{}{}", "!".repeat(times), ident)
            }

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

impl Debug for ValueSource<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        Display::fmt(self, f)
    }
}

#[derive(Clone, Copy)]
pub enum Out<'code> {
    Ident(VarIdent<'code>),
    Derefs(Derefs<'code>),
}

impl Display for Out<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Self::Ident(ident) => write!(f, "{}", ident),
            Self::Derefs(Derefs { ident, times }) => {
                write!(f, "{}{}", "!".repeat(times), ident)
            }
        }
    }
}

impl Debug for Out<'_> {
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
    Value { value: ValueSource<'code>, out: Option<Out<'code>> },
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

pub fn derefs(input: &str) -> IResult<&str, Derefs> {
    let (input, times) = map(many1(wtag("!")), |v| v.len())(input)?;

    map(cut(var_ident), move |ident| Derefs { ident, times })(input)
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
        fn args(input: &str) -> IResult<&str, Vec<ValueSource>> {
            delimited(wtag("("), cut(values), cut(wtag(")")))(input)
        }

        let (input, ident) = var_ident(input)?;

        Ok(match args(input) {
            Err(_) => (input, ValueSource::Variable(ident)),
            Ok((input, args)) => (input, ValueSource::FunctionCall { ident, args }),
        })
    }

    fn take_ref(input: &str) -> IResult<&str, ValueSource> {
        preceded(wtag("?"), cut(map(var_ident, ValueSource::TakeRef)))(input)
    }

    fn derefs_ident(input: &str) -> IResult<&str, ValueSource> {
        map(derefs, ValueSource::Derefs)(input)
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

    alt((keyword_value, var, take_ref, derefs_ident, number, string))(input)
}

pub fn values(input: &str) -> IResult<&str, Vec<ValueSource>> {
    many0(value)(input)
}

pub fn instruction(input: &str) -> IResult<&str, Instruction> {
    fn instruction_out(input: &str) -> IResult<&str, Out> {
        fn out_ident(input: &str) -> IResult<&str, Out> {
            map(var_ident, Out::Ident)(input)
        }

        fn out_derefs(input: &str) -> IResult<&str, Out> {
            map(derefs, Out::Derefs)(input)
        }

        preceded(wtag(">"), cut(alt((out_ident, out_derefs))))(input)
    }

    fn write_value(input: &str) -> IResult<&str, Instruction> {
        let (input, (value, out)) = pair(value, opt(instruction_out))(input)?;

        Ok((input, Instruction::Value { value, out }))
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

        Ok((input, Instruction::FunctionDefinition(FunctionDefinition { signature, body })))
    }

    let (input, instruction) =
        (write_value, label_definition, go, goif, goifn, ret, function_definition).choice(input)?;
    Ok((input, instruction))
}

pub fn instructions(input: &str) -> IResult<&str, Vec<Instruction>> {
    terminated(many0(instruction), cut(spaces))(input)
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Trace;

impl Trace {
    pub fn new() -> Self {
        println!("[Trace::new]");
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
    Ref(ValueRef<'code>),
    List(Vec<ValueRef<'code>>),
}

impl Value<'_> {
    pub fn as_index(&self) -> usize {
        let res = self.as_number().copied().expect("expected a number");

        if (res.floor() - res).abs() > 1e-2 {
            panic!("expected an integer");
        }

        res as usize - 1
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

impl<'code> From<Option<Value<'code>>> for Value<'code> {
    fn from(value: Option<Value<'code>>) -> Self {
        match value {
            Some(value) => value,
            None => Self::None,
        }
    }
}

impl Display for Value<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Self::None => write!(f, "none"),
            Self::Bool(bool) => write!(f, "{}", bool),
            Self::Number(number) => write!(f, "{}", number),
            Self::String(ref string) => write!(f, "{}", string),
            Self::Trace(ref trace) => write!(f, "{}", trace),
            Self::Ref(ref value) => write!(f, "?{}", value.borrow()),

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
            (Self::Ref(lhs), Self::Ref(rhs)) => lhs.partial_cmp(rhs),
            (Self::Trace(_), Self::Trace(_)) => Some(Ordering::Equal),
            (Self::List(lhs), Self::List(rhs)) => lhs.partial_cmp(rhs),

            (
                Self::None
                | Self::Bool(_)
                | Self::Number(_)
                | Self::String(_)
                | Self::Ref(_)
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

#[derive(Debug, Clone, Copy)]
pub struct CompileError;

#[derive(Debug, Clone, Copy)]
pub struct UnknownVariable;

pub struct Instructions<'code> {
    global: Vec<Instruction<'code>>,
    locals: Vec<NonNull<[Instruction<'code>]>>,
    _pin: PhantomPinned,
}

impl<'code> Instructions<'code> {
    pub fn new(global: Vec<Instruction<'code>>) -> Pin<Box<RefCell<Self>>> {
        Box::pin(Self { global, locals: Default::default(), _pin: PhantomPinned }.into())
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
        self.local().map(|local| local.iter().rev()).into_iter().flatten()
    }

    fn all_rev(&self) -> impl Iterator<Item = &Instruction<'code>> {
        self.local_rev().chain(self.global_rev())
    }

    pub fn run(&mut self, vm: &mut VM<'code>) {
        let mut instruction_idx = 0;

        'run: while instruction_idx < self.current().len() {
            let instruction = &self.current()[instruction_idx];
            instruction_idx += 1;

            match *instruction {
                Instruction::Value { ref value, out } => {
                    let value = vm.value(self, value.clone());

                    if let Some(out) = out {
                        match out {
                            Out::Ident(ident) => {
                                match ident {
                                    VarIdent::Global(ident) => vm.globals.insert(ident, value),
                                    VarIdent::Local(ident) => vm.locals_mut().insert(ident, value),
                                };
                            }

                            Out::Derefs(derefs) => {
                                vm.derefs_mut(derefs, |out| *out = value);
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
    ) -> Value<'code> {
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
                                let value = vm.value(self, value);
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

                    let ref_ = out_value.borrow();
                    ref_.clone()
                } else {
                    Value::None
                }
            }
        }
    }

    pub fn call_native_function(
        &mut self,
        vm: &mut VM<'code>,
        ident: Ident<'code>,
        args: Vec<ValueSource<'code>>,
    ) -> Value<'code> {
        let mut args = args.into_iter().map(|value| vm.value(self, value));

        match ident.into() {
            "sum" => {
                let mut res = 0.;
                for arg in args {
                    res += arg.borrow().as_number().expect("expected a number");
                }

                Value::Number(res)
            }

            "sub" => {
                let mut res = *args
                    .next()
                    .expect("expected an argument")
                    .borrow()
                    .as_number()
                    .expect("expected a number");

                for arg in args {
                    res -= arg.borrow().as_number().expect("expected a number");
                }

                Value::Number(res)
            }

            "join" => {
                let mut res = String::default();
                for arg in args {
                    write!(&mut res, "{}", arg.borrow()).unwrap();
                }

                Value::String(Cow::Owned(res))
            }

            "prin" => {
                for arg in args {
                    print!("{}", arg.borrow());
                }

                Value::None
            }

            "print" => {
                for arg in args {
                    print!("{}", arg.borrow());
                }
                println!();

                Value::None
            }

            "cmp" => {
                let (a, b) = args.collect_tuple().expect("invalid arguments count");

                a.partial_cmp(&b).map(Value::from).into()
            }

            // TODO?: a.partial_cmp(b).expect("cannot compare values of different types")
            "eq" => Value::Bool(args.tuple_windows().all(|(a, b)| a == b)),
            "ne" => Value::Bool(args.tuple_windows().all(|(a, b)| a != b)),
            "lt" => Value::Bool(args.tuple_windows().all(|(a, b)| a < b)),
            "gt" => Value::Bool(args.tuple_windows().all(|(a, b)| a > b)),
            "le" => Value::Bool(args.tuple_windows().all(|(a, b)| a <= b)),
            "ge" => Value::Bool(args.tuple_windows().all(|(a, b)| a >= b)),

            "list" => Value::List(args.collect()),

            "at" => {
                let list_or_ref = args.next().expect("expected an argument");
                let list_or_ref = list_or_ref.borrow();
                let list_or_ref = &*list_or_ref;

                let idx = args.next().expect("expected an argument").borrow().as_index();

                match list_or_ref {
                    Value::Ref(list_ref) => match &*list_ref.borrow() {
                        Value::List(list) => Value::Ref(list[idx].clone()),
                        _ => panic!("expected a list or a reference to list"),
                    },

                    Value::List(list) => list[idx].borrow().clone(),

                    _ => panic!("expected a list or a reference to list"),
                }
            }

            "push" => {
                let list = args.next().expect("expected an argument");
                let list = list.borrow();
                let list = list.as_ref().expect("expected a reference");
                let mut list = list.borrow_mut();
                let list = list.as_list_mut().expect("expected a reference to list");

                list.extend(args);

                Value::None
            }

            "trace" => Value::Trace(Trace::new()),

            _ => panic!("function '{}' was not found", ident),
        }
    }
}

#[derive(Default)]
pub struct VM<'code> {
    pub globals: HashMap<Ident<'code>, ValueRef<'code>>,
    locals: Vec<HashMap<Ident<'code>, ValueRef<'code>>>,
}

impl<'code> VM<'code> {
    pub fn run(&mut self, code: &'code str) -> Result<(), CompileError> {
        let (_, instructions) = instructions(code).unwrap();

        let instructions = Instructions::new(instructions);
        instructions.borrow_mut().run(self);

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

    pub fn value(
        &mut self,
        instructions: &mut Instructions<'code>,
        value: ValueSource<'code>,
    ) -> ValueRef<'code> {
        match value {
            ValueSource::Variable(ident) => {
                self.var(ident).expect("expected the variable to exist").clone()
            }

            ValueSource::Literal(ref literal) => value_ref(literal.clone()),

            ValueSource::TakeRef(ident) => value_ref(Value::Ref(
                self.var(ident).expect("expected the variable to exist").clone(),
            )),

            ValueSource::Derefs(derefs) => self.derefs(derefs).clone(),

            ValueSource::FunctionCall { ident, args } => {
                value_ref(instructions.call_function(self, ident, args))
            }
        }
    }

    pub fn derefs(&self, Derefs { ident, times }: Derefs<'code>) -> ValueRef<'code> {
        let mut res = self.var(ident).expect("expected the variable to exist").clone();

        for _ in 1..=times {
            let new_res = (*res.borrow()).as_ref().cloned().expect("expected a reference");
            res = new_res;
        }

        res
    }

    pub fn derefs_mut(
        &mut self,
        Derefs { ident, times }: Derefs<'code>,
        f: impl FnOnce(&mut ValueRef<'code>),
    ) {
        fn rec<'code>(
            value: &mut ValueRef<'code>,
            times: usize,
            f: impl FnOnce(&mut ValueRef<'code>),
        ) {
            if times == 0 {
                f(value);
            } else {
                let mut borrow = value.borrow_mut();
                rec(borrow.as_ref_mut().expect("expected a reference"), times - 1, f)
            }
        }

        let res = self.var_mut(ident).expect("expected the variable to exist");
        rec(res, times, f);
    }
}

fn main() {
    let mut vm = VM::default();

    vm.run(include_str!("code.bar")).unwrap();
}
