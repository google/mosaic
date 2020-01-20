use clang::{Entity, Type};

pub(crate) const INDENT_SPACES: usize = 2;

macro_rules! print_indent {
    ($self: expr, $fmt:expr) => { print_indent!($self, $fmt,) };
    ($self: expr, $fmt:expr, $($arg:expr),*) => {
        print!(concat!("{:indent$}", $fmt), "", $($arg),*,
               indent=$self.indent * $crate::util::INDENT_SPACES)
    };
}

macro_rules! println_indent {
    ($self:expr, $fmt:expr) => { println_indent!($self, $fmt,) };
    ($self:expr, $fmt:expr, $($arg:expr),*) => {
        println!(concat!("{:indent$}", $fmt), "", $($arg),*,
                 indent=$self.indent * $crate::util::INDENT_SPACES)
    };
}

pub(crate) trait DisplayName {
    fn display_name(&self) -> String;
    fn display_name_or(&self, alt: &'static str) -> String;
}

impl<'a> DisplayName for Entity<'a> {
    fn display_name(&self) -> String {
        self.display_name_or("(unnamed)")
    }
    fn display_name_or(&self, alt: &'static str) -> String {
        self.get_display_name().unwrap_or_else(|| alt.to_string())
    }
}

impl<'a> DisplayName for Type<'a> {
    fn display_name(&self) -> String {
        self.get_display_name()
    }
    fn display_name_or(&self, _alt: &'static str) -> String {
        self.get_display_name()
    }
}
