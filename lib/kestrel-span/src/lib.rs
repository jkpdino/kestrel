pub use codespan_reporting::files::SimpleFile;

pub type Span = std::ops::Range<usize>;

#[derive(Debug, Clone, PartialEq)]
pub struct Spanned<T> {
    pub value: T,
    pub span: Span,
}

impl<T> Spanned<T> {
    pub fn new(value: T, span: Span) -> Self {
        Self { value, span }
    }

    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> Spanned<U> {
        Spanned {
            value: f(self.value),
            span: self.span,
        }
    }

    pub fn as_ref(&self) -> Spanned<&T> {
        Spanned {
            value: &self.value,
            span: self.span.clone(),
        }
    }
}

pub type SourceFile = SimpleFile<String, String>;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_spanned() {
        let spanned = Spanned::new(42, 0..2);
        assert_eq!(spanned.value, 42);
        assert_eq!(spanned.span, 0..2);
    }

    #[test]
    fn test_map() {
        let spanned = Spanned::new(42, 0..2);
        let mapped = spanned.map(|x| x * 2);
        assert_eq!(mapped.value, 84);
        assert_eq!(mapped.span, 0..2);
    }
}
