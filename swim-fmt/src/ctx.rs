use std::rc::Rc;

use crate::{
    config::{FormatterConfig, FormatterConfigBuilder},
    Line,
};
use swim_parse::SymbolInterner;

pub struct FormatterContext {
    pub interner: SymbolInterner,
    pub config: FormatterConfig,
    indentation: usize,
}

impl FormatterContext {
    pub fn from_interner(interner: SymbolInterner) -> Self {
        Self {
            interner,
            config: FormatterConfigBuilder::default().build(),
            indentation: 0,
        }
    }
    pub fn new_line(&self, content: impl AsRef<str>) -> Line {
        Line {
            content: Rc::from(content.as_ref()),
            indentation: self.indentation,
        }
    }

    pub fn tab_in(&mut self) {
        self.indentation += 2;
    }

    pub fn tab_out(&mut self) {
        self.indentation -= 2;
    }

    pub fn with_config(self, config: FormatterConfig) -> FormatterContext {
        FormatterContext { config, ..self }
    }

    pub(crate) fn indentation(&self) -> usize {
        self.indentation
    }
}
