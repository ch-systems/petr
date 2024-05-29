use error_printing::SourcedItem;
use miette::{Diagnostic, LabeledSpan, SourceSpan};

#[derive(PartialEq, Eq, Clone)]
pub struct SpannedItem<T>(T, Span);

impl<T> SpannedItem<T> {
    pub fn item(&self) -> &T {
        &self.0
    }
    pub fn into_item(self) -> T {
        self.0
    }
    pub fn span(&self) -> Span {
        self.1
    }

    fn with_source(
        self,
        name: impl Into<String>,
        source: impl Into<String>,
    ) -> SourcedItem<SpannedItem<T>>
    where
        T: Diagnostic,
    {
        SourcedItem::new(name, source, self)
    }
}

impl<T: std::fmt::Display> std::fmt::Display for SpannedItem<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.item())
    }
}
impl<T: std::error::Error> std::error::Error for SpannedItem<T> {}

impl<T: Diagnostic + std::error::Error> Diagnostic for SpannedItem<T> {
    fn code<'a>(&'a self) -> Option<Box<dyn std::fmt::Display + 'a>> {
        self.item().code()
    }

    fn severity(&self) -> Option<miette::Severity> {
        self.item().severity()
    }

    fn help<'a>(&'a self) -> Option<Box<dyn std::fmt::Display + 'a>> {
        self.item().help()
    }

    fn url<'a>(&'a self) -> Option<Box<dyn std::fmt::Display + 'a>> {
        self.item().url()
    }

    fn labels(&self) -> Option<Box<dyn Iterator<Item = miette::LabeledSpan> + '_>> {
        let span = self.span().span();
        let label = self.item().to_string();
        let labeled_span = LabeledSpan::new_with_span(Some(label), span);
        Some(Box::new(std::iter::once(labeled_span)))
    }

    fn related<'a>(&'a self) -> Option<Box<dyn Iterator<Item = &'a dyn Diagnostic> + 'a>> {
        self.item().related()
    }

    fn diagnostic_source(&self) -> Option<&dyn Diagnostic> {
        self.item().diagnostic_source()
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

    /// goes from the `hi` of self to the `hi` of `after_span`
    pub fn hi_to_hi(&self, after_span: Span) -> Self {
        assert!(
            self.source == after_span.source,
            "cannot join spans from different files"
        );
        let lo = self.span.offset() + self.span.len();
        let hi = after_span.span.offset() + after_span.span.len();
        Self {
            source: self.source,
            span: SourceSpan::new(lo.into(), hi - lo),
        }
    }

    pub fn span(&self) -> miette::SourceSpan {
        self.span
    }

    pub fn source(&self) -> SourceId {
        self.source
    }
}

pub mod error_printing {

    use crate::{IndexMap, SourceId, SpannedItem};
    use miette::{Diagnostic, Error, LabeledSpan, MietteDiagnostic, NamedSource, Report};
    use thiserror::Error;

    // #[derive(Error, Debug)]
    // struct ErrorWithSource<'a, T> where T: Diagnostic {
    //     span: miette::SourceSpan,
    //     source: &'a str,
    //     diagnostic: T,
    //     severity: miette::Severity,
    //     help: Option<String>,
    // }

    // impl <T: Diagnostic> std::fmt::Display for ErrorWithSource<'_, T> {
    //     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    //         todo!()
    //     }
    // }

    // impl <'a, T: Diagnostic> Diagnostic for ErrorWithSource<'a, T> {
    //     fn code<'b>(&'b self) -> Option<Box<dyn std::fmt::Display + 'b>> {
    //         None
    //     }

    //     fn severity(&self) -> Option<miette::Severity> {
    //         None
    //     }

    //     fn help<'b> (&'b self) -> Option<Box<dyn std::fmt::Display + 'b>> {
    //         self.help.map(|x| -> Box<dyn std::fmt::Display> { Box::new(x) })
    //     }

    //     fn url<'b>(&'b self) -> Option<Box<dyn std::fmt::Display + 'b>> {
    //         None
    //     }

    //     fn source_code(&self) -> Option<&dyn miette::SourceCode> {
    //        Some(&self.source.to_string())
    //     }

    //     fn labels(&self) -> Option<Box<dyn Iterator<Item = miette::LabeledSpan> + '_>> {
    //         let span = LabeledSpan::new_with_span(self., self.span)
    //         Some(self.span)
    //     }

    //     fn related<'b>(&'b self) -> Option<Box<dyn Iterator<Item = &'b dyn Diagnostic> + 'b>> {
    //         None
    //     }

    //     fn diagnostic_source(&self) -> Option<&dyn Diagnostic> {
    //         None
    //     }
    //   }

    //    pub type Source<'a> = &'a str;
    #[derive(Debug)]
    pub(crate) struct SourcedItem<T>
    where
        T: Diagnostic + std::error::Error + std::fmt::Debug,
    {
        name: String,
        source: String,
        item: T,
    }
    impl<T: Diagnostic> SourcedItem<T> {
        pub(crate) fn new(
            name: impl Into<String>,
            source: impl Into<String>,
            item: SpannedItem<T>,
        ) -> SourcedItem<SpannedItem<T>> {
            SourcedItem {
                name: name.into(),
                source: source.into(),
                item,
            }
        }
    }

    impl<T: Diagnostic> Diagnostic for SourcedItem<T> {
        fn code<'a>(&'a self) -> Option<Box<dyn std::fmt::Display + 'a>> {
            self.item.code()
        }

        fn severity(&self) -> Option<miette::Severity> {
            self.item.severity()
        }

        fn help<'a>(&'a self) -> Option<Box<dyn std::fmt::Display + 'a>> {
            self.item.help()
        }

        fn url<'a>(&'a self) -> Option<Box<dyn std::fmt::Display + 'a>> {
            self.item.url()
        }

        fn source_code(&self) -> Option<&dyn miette::SourceCode> {
            Some(&self.source)
        }

        fn labels(&self) -> Option<Box<dyn Iterator<Item = LabeledSpan> + '_>> {
            self.item.labels()
        }

        fn related<'a>(&'a self) -> Option<Box<dyn Iterator<Item = &'a dyn Diagnostic> + 'a>> {
            self.item.related()
        }

        fn diagnostic_source(&self) -> Option<&dyn Diagnostic> {
            self.item.diagnostic_source()
        }
    }

    impl<T> std::fmt::Display for SourcedItem<T>
    where
        T: Diagnostic + std::error::Error + std::fmt::Debug,
    {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{}", self.item)
        }
    }

    impl<T: std::error::Error> std::error::Error for SourcedItem<T> where
        T: Diagnostic + std::error::Error + std::fmt::Debug
    {
    }

    pub fn render<'b, T>(
        sources: &'b IndexMap<SourceId, (&'static str, &'static str)>,
        err: SpannedItem<T>,
    ) where
        T: miette::Diagnostic + Send + Sync + 'static,
    {
        let span = err.span();
        let (name, source) = sources.get(span.source());
        let sourced_item = err.with_source(*name, *source);
        eprintln!("{:?}", Report::new(sourced_item));
    }
}
