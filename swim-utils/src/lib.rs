pub use common_types::*;
pub use index_map::IndexMap;
pub use sources::{error_printing::render as render_error, SourceId, Span, SpannedItem};

mod common_types;
mod index_map;
mod sources;
mod pretty_print {
    //! Trait for pretty-printing compiler internals
    //! Primarily used for testing and debugging purposes

    use crate::{Identifier, SpannedItem, SymbolInterner};
    pub trait PrettyPrint {
        fn pretty_print(
            &self,
            interner: &SymbolInterner,
            indentation: usize,
        ) -> String;
    }
    impl PrettyPrint for Identifier {
        fn pretty_print(
            &self,
            interner: &SymbolInterner,
            _: usize,
        ) -> String {
            interner.get(self.id).to_string()
        }
    }
    impl<T> PrettyPrint for SpannedItem<T>
    where
        T: PrettyPrint,
    {
        fn pretty_print(
            &self,
            interner: &SymbolInterner,
            indentation: usize,
        ) -> String {
            self.item().pretty_print(interner, indentation)
        }
    }
}
pub use pretty_print::PrettyPrint;
