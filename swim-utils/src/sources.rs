use miette::SourceSpan;

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

    pub fn join(&self, after_span: Span) -> Span {
        assert!(
            self.source == after_span.source,
            "cannot join spans from different files"
        );

        let (first_span, second_span) = if self.span.offset() < after_span.span.offset() {
            (self.span, after_span.span)
        } else {
            (after_span.span, self.span)
        };

        let (first_end, second_end) = (
            first_span.len() + first_span.offset(),
            second_span.len() + second_span.offset(),
        );

        let end = std::cmp::max(first_end, second_end);

        let length = end - first_span.offset();

        Self {
            source: self.source,
            span: SourceSpan::new(first_span.offset().into(), length),
        }
    }
}
