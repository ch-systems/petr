// TODO:
// make Commented a struct with a comment field
// impl Spanned for Commented<SpannedItem<T>> and Commentable for SpannedItem<Commented<T>>
// impl Spanned for most AST nodes

use crate::Comment;

pub struct Commented<T>(T, Vec<Comment>);

impl<T> Commented<T> {
    pub fn new(item: T, comments: Vec<Comment>) -> Self {
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
