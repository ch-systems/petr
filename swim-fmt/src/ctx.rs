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

    /// indent by the default tab size
    pub fn indented<F, T>(&mut self, f: F) -> T
    where
        F: FnOnce(&mut Self) -> T,
    {
        self.indentation += self.config.tab_size();
        let res = f(self);
        self.indentation -= self.config.tab_size();
        res
    }

    pub fn indent_by<F, T>(&mut self, indentation: usize, f: F) -> T
    where
        F: FnOnce(&mut Self) -> T,
    {
        self.indentation += indentation;
        let res = f(self);
        self.indentation -= indentation;
        res
    }

    pub fn with_config(self, config: FormatterConfig) -> FormatterContext {
        FormatterContext { config, ..self }
    }

    pub(crate) fn indentation(&self) -> usize {
        self.indentation
    }
}
