use {
    core::fmt,
    std::fmt::Display,
};

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
