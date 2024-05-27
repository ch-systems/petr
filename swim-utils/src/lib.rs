/// If some item has a span associated with it, then it is `Spanned`.
pub trait Spanned {
    fn span(&self) -> Span;
}
impl<T> Spanned for SpannedItem<T> {
    fn span(&self) -> Span {
        self.1
    }
}

pub struct SpannedItem<T>(T, Span);

#[derive(Clone, Copy)]
pub struct SourceId(usize);

impl From<usize> for SourceId {
    fn from(other: usize) -> SourceId {
        SourceId(other)
    }
}
impl From<SourceId> for usize {
    fn from(other: SourceId) -> usize {
        other.0
    }
}

#[derive(Clone, Copy)]
pub struct Span {
    source: SourceId,
    span: miette::SourceSpan,
}

impl Span {
    pub fn new(source: SourceId, span: miette::SourceSpan) -> Self {
        Self { source, span }
    }
    pub fn with_item<T>(self, item: T) -> SpannedItem<T> {
        SpannedItem(item, self)
    }
}
