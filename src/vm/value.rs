use {
    super::GcValue,
    crate::Trace,
    core::fmt,
    enum_as_inner::EnumAsInner,
    std::{
        borrow::Cow,
        cmp::Ordering,
        fmt::Display,
        num::FpCategory,
    },
};

#[derive(Debug, Clone, PartialEq, EnumAsInner)]
pub enum Value<'code> {
    None,
    Bool(bool),
    Number(f64),
    String(Cow<'code, str>),
    Trace(Trace),
    List(Vec<GcValue<'code>>),
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
