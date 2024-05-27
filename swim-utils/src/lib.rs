/// If some item has a span associated with it, then it is `Spanned`.
pub trait Spanned {
    fn span(&self) -> Span;
}
impl<T> Spanned for SpannedItem<T> {
    fn span(&self) -> Span {
        self.1
    }
}

#[derive(PartialEq, Eq, Clone)]
pub struct SpannedItem<T>(T, Span);

impl<T> SpannedItem<T> {
    pub fn item(&self) -> &T {
        &self.0
    }
}

impl<T> Copy for SpannedItem<T> where T: Copy {}

impl<T> std::fmt::Debug for SpannedItem<T>
where
    T: std::fmt::Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "SpannedItem {:?} [{:?}]", self.0, self.1)
    }
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
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

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
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
