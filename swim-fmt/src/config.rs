pub struct FormatterConfig {
    put_fn_params_on_new_lines: bool,
    use_set_notation_for_types: bool,
    join_comments: bool,
    newlines_between_items: usize,
    newlines_between_comment_and_item: usize,
    put_variants_on_new_lines: bool,
    tab_size: usize,
}

impl FormatterConfig {
    pub fn newlines_between_items(&self) -> usize {
        self.newlines_between_items
    }

    pub fn put_fn_params_on_new_lines(&self) -> bool {
        self.put_fn_params_on_new_lines
    }

    pub fn use_set_notation_for_types(&self) -> bool {
        self.use_set_notation_for_types
    }

    pub fn join_comments(&self) -> bool {
        self.join_comments
    }

    pub fn newlines_between_comment_and_item(&self) -> usize {
        self.newlines_between_comment_and_item
    }

    pub fn put_variants_on_new_lines(&self) -> bool {
        self.put_variants_on_new_lines
    }

    pub fn tab_size(&self) -> usize {
        self.tab_size
    }
}

impl Default for FormatterConfig {
    fn default() -> Self {
        FormatterConfigBuilder::default().build()
    }
}

pub struct FormatterConfigBuilder {
    put_fn_params_on_new_lines: bool,
    use_set_notation_for_types: bool,
    join_comments: bool,
    newlines_between_items: usize,
    newlines_between_comment_and_item: usize,
    put_variants_on_new_lines: bool,
    tab_size: usize,
}

impl FormatterConfigBuilder {
    pub fn put_fn_params_on_new_lines(self, put_fn_params_on_new_lines: bool) -> Self {
        Self {
            put_fn_params_on_new_lines,
            ..self
        }
    }

    pub fn use_set_notation_for_types(self, use_set_notation_for_types: bool) -> Self {
        Self {
            use_set_notation_for_types,
            ..self
        }
    }

    pub fn join_comments(self, join_comments: bool) -> Self {
        Self {
            join_comments,
            ..self
        }
    }

    pub fn newlines_between_items(self, newlines_between_items: usize) -> Self {
        Self {
            newlines_between_items,
            ..self
        }
    }

    pub fn newlines_between_comment_and_item(
        self,
        newlines_between_comment_and_item: usize,
    ) -> Self {
        Self {
            newlines_between_comment_and_item,
            ..self
        }
    }

    pub fn put_variants_on_new_lines(self, put_variants_on_new_lines: bool) -> Self {
        Self {
            put_variants_on_new_lines,
            ..self
        }
    }

    pub fn tab_size(self, tab_size: usize) -> Self {
        Self { tab_size, ..self }
    }

    pub fn build(self) -> FormatterConfig {
        FormatterConfig {
            put_fn_params_on_new_lines: self.put_fn_params_on_new_lines,
            use_set_notation_for_types: self.use_set_notation_for_types,
            join_comments: self.join_comments,
            newlines_between_items: self.newlines_between_items,
            newlines_between_comment_and_item: self.newlines_between_comment_and_item,
            put_variants_on_new_lines: self.put_variants_on_new_lines,
            tab_size: self.tab_size,
        }
    }
}

impl Default for FormatterConfigBuilder {
    fn default() -> Self {
        Self {
            put_fn_params_on_new_lines: true,
            use_set_notation_for_types: true,
            join_comments: true,
            newlines_between_items: 1,
            newlines_between_comment_and_item: 0,
            put_variants_on_new_lines: true,
            tab_size: 2,
        }
    }
}
