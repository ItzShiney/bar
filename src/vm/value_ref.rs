use {
    super::Value,
    std::{
        cell::RefCell,
        rc::Rc,
    },
};

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
