use {
    super::GcValue,
    crate::{
        vm::Value,
        Trace,
    },
    itertools::{
        EitherOrBoth,
        Itertools,
    },
    std::{
        borrow::Cow,
        collections::HashMap,
        fmt::Write,
    },
};

mod instruction;
mod parser;

use {
    super::VM,
    std::ptr::NonNull,
};
pub use {
    instruction::*,
    parser::*,
};

pub struct Instructions<'code> {
    global: Box<[Instruction<'code>]>,
    locals: Vec<NonNull<[Instruction<'code>]>>,
}

impl<'code> From<Box<[Instruction<'code>]>> for Instructions<'code> {
    fn from(global: Box<[Instruction<'code>]>) -> Self {
        Self {
            global,
            locals: Default::default(),
        }
    }
}

impl<'code> Instructions<'code> {
    pub fn new(global: Vec<Instruction<'code>>) -> Self {
        let global = global.into_boxed_slice();

        Self {
            global,
            locals: Default::default(),
        }
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
        self.local()
            .map(|local| local.iter().rev())
            .unwrap_or_default()
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
                                .expect(
                                    "expected global variable 'if' to exist, since it is used by \
                                     'goif' statement",
                                )
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
                                .expect(
                                    "expected global variable 'if' to exist, since it is used by \
                                     'goifn' statement",
                                )
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
                            Instruction::LabelDefinition {
                                ident: definition_ident,
                            } if label == definition_ident => {
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
        locals: HashMap<&'code str, GcValue<'code>>,
        ident: VarIdent<'code>,
    ) -> HashMap<&'code str, GcValue<'code>> {
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
                            signature:
                                FunctionSignature {
                                    ident: definition_ident,
                                    ..
                                },
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
    ) -> GcValue<'code> {
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
                        panic!(
                            "expected local variable '{}' to exist, since it is being returned",
                            out_ident
                        );
                    };

                    out_value
                } else {
                    GcValue::none()
                }
            }
        }
    }

    pub fn call_native_function(
        &mut self,
        vm: &mut VM<'code>,
        ident: Ident<'code>,
        args: Vec<ValueSource<'code>>,
    ) -> GcValue<'code> {
        let mut args = args.into_iter().map(|value| vm.value(self, &value));

        fn next_arg<'code>(args: &mut impl Iterator<Item = GcValue<'code>>) -> GcValue<'code> {
            args.next().expect("expected an argument")
        }

        fn as_number<'code>(arg: GcValue<'code>) -> f64 {
            arg.borrow()
                .as_number()
                .copied()
                .expect("expected a number")
        }

        fn as_index<'code>(arg: GcValue<'code>) -> usize {
            arg.borrow().as_index().expect("expected an index")
        }

        fn assert_empty<'code>(mut args: impl Iterator<Item = GcValue<'code>>) {
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

        let res: GcValue<'_> = match ident {
            "cp" => verify!(
                GcValue::new(next_arg(&mut args).borrow().clone());
                assert_empty(args);
            ),

            "num" => verify!(
                GcValue::new(next_arg(&mut args).borrow().as_string().expect("expected a string").parse::<f64>().expect("failed to parse the string into a number").into());
                assert_empty(args);
            ),

            "sum" => {
                let mut res = 0.;
                for arg in args {
                    res += as_number(arg);
                }

                GcValue::new(res.into())
            }

            "sub" => {
                let mut res = as_number(next_arg(&mut args));

                for arg in args {
                    res -= as_number(arg);
                }

                GcValue::new(res.into())
            }

            "join" => {
                let mut res = String::default();
                for arg in args {
                    write!(&mut res, "{}", arg.borrow()).unwrap();
                }

                GcValue::new(Value::String(Cow::Owned(res)))
            }

            "prin" => {
                for arg in args {
                    print!("{}", arg.borrow());
                }

                GcValue::none()
            }

            "print" => {
                for arg in args {
                    print!("{}", arg.borrow());
                }
                println!();

                GcValue::none()
            }

            "cmp" => {
                let (a, b) = args.collect_tuple().expect("expected exactly 2 arguments");

                GcValue::new(a.partial_cmp(&b).map(Value::from).into())
            }

            "is" => {
                let (a, b) = args.collect_tuple().expect("expected exactly 2 arguments");

                GcValue::new(a.ptr_eq(&b).into())
            }

            "list" => GcValue::new(Value::List(args.collect())),

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

                GcValue::new(list.extend(args).into())
            }

            "pop" => {
                let list = next_arg(&mut args);
                let_borrow!(mut list);
                let_as_list!(mut list);

                match list.pop().into() {
                    None => GcValue::none(),
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
                GcValue::new(Value::Trace(Trace::default()));
                assert_empty(args);
            ),

            _ => {
                fn get_cmp<'code>(
                    ident: Ident<'code>,
                ) -> Option<fn(GcValue<'code>, GcValue<'code>) -> bool> {
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
                    GcValue::new(Value::Bool(args.tuple_windows().all(|(a, b)| cmp(a, b))))
                } else {
                    panic!("function '{}' was not found", ident);
                }
            }
        };

        res
    }
}
