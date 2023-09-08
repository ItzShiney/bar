use {
    super::Value,
    std::{
        cell::{
            self,
            RefCell,
        },
        fmt::{
            Debug,
            Display,
        },
        rc::Rc,
    },
};

#[derive(Clone, PartialEq, PartialOrd)]
pub struct GcValue<'code>(Rc<RefCell<Value<'code>>>);

impl<'code> GcValue<'code> {
    pub fn new(value: Value<'code>) -> Self {
        Self(Rc::new(value.into()))
    }

    pub fn borrow(&self) -> cell::Ref<Value<'code>> {
        self.0.borrow()
    }

    pub fn borrow_mut(&self) -> cell::RefMut<Value<'code>> {
        self.0.borrow_mut()
    }

    pub fn none() -> GcValue<'code> {
        GcValue::new(Value::None)
    }

    pub fn cp(&self) -> GcValue<'code> {
        GcValue::new(self.borrow().clone())
    }
}

impl Display for GcValue<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&*self.borrow(), f)
    }
}

impl Debug for GcValue<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(&*self.borrow(), f)
    }
}
