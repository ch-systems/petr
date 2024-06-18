use swim_utils::{PrettyPrint, SymbolInterner};

use crate::Comment;

pub struct Commented<T>(T, Vec<Comment>);

impl<T> Commented<T> {
    pub fn new(
        item: T,
        comments: Vec<Comment>,
    ) -> Self {
        Commented(item, comments)
    }

    pub fn item(&self) -> &T {
        &self.0
    }

    pub fn comments(&self) -> &[Comment] {
        &self.1[..]
    }
}

impl<T: Clone> Clone for Commented<T> {
    fn clone(&self) -> Self {
        Commented(self.0.clone(), self.1.clone())
    }
}

impl PrettyPrint for Comment {
    fn pretty_print(
        &self,
        _interner: &SymbolInterner,
        indentation: usize,
    ) -> String {
        format!("{}{{- {} -}}", "  ".repeat(indentation), self.content)
    }
}

impl<T> PrettyPrint for Commented<T>
where
    T: PrettyPrint,
{
    fn pretty_print(
        &self,
        interner: &SymbolInterner,
        indentation: usize,
    ) -> String {
        let comments = self.comments();
        format!(
            "{}{}",
            if comments.is_empty() {
                String::new()
            } else {
                format!(
                    "{}\n",
                    comments
                        .iter()
                        .map(|comment| comment.pretty_print(interner, indentation))
                        .collect::<Vec<_>>()
                        .join("\n")
                )
            },
            self.item().pretty_print(interner, indentation)
        )
    }
}
