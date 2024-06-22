#[derive(Debug, Clone, Copy)]
pub struct FormatterConfig {
    put_fn_params_on_new_lines: bool,
    use_set_notation_for_types: bool,
    join_comments: bool,
    newlines_between_items: usize,
    newlines_between_comment_and_item: usize,
    put_variants_on_new_lines: bool,
    put_list_elements_on_new_lines: bool,
    put_fn_body_on_new_line: bool,
    tab_size: usize,
    max_line_length: usize,
    put_fn_args_on_new_lines: bool,
    put_trailing_commas_on_let_bindings: bool,
    backup: bool,
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

    pub fn put_list_elements_on_new_lines(&self) -> bool {
        self.put_list_elements_on_new_lines
    }

    pub fn put_fn_body_on_new_line(&self) -> bool {
        self.put_fn_body_on_new_line
    }

    pub fn tab_size(&self) -> usize {
        self.tab_size
    }

    pub fn max_line_length(&self) -> usize {
        self.max_line_length
    }

    pub fn put_fn_args_on_new_lines(&self) -> bool {
        self.put_fn_args_on_new_lines
    }

    pub fn put_trailing_commas_on_let_bindings(&self) -> bool {
        self.put_trailing_commas_on_let_bindings
    }

    pub fn backup(&self) -> bool {
        self.backup
    }

    pub(crate) fn as_builder(&self) -> FormatterConfigBuilder {
        FormatterConfigBuilder {
            put_fn_params_on_new_lines: self.put_fn_params_on_new_lines,
            use_set_notation_for_types: self.use_set_notation_for_types,
            join_comments: self.join_comments,
            newlines_between_items: self.newlines_between_items,
            newlines_between_comment_and_item: self.newlines_between_comment_and_item,
            put_variants_on_new_lines: self.put_variants_on_new_lines,
            put_list_elements_on_new_lines: self.put_list_elements_on_new_lines,
            put_fn_body_on_new_line: self.put_fn_body_on_new_line,
            tab_size: self.tab_size,
            max_line_length: self.max_line_length,
            put_fn_args_on_new_lines: self.put_fn_args_on_new_lines,
            put_trailing_commas_on_let_bindings: self.put_trailing_commas_on_let_bindings,
            backup: self.backup,
        }
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
    put_list_elements_on_new_lines: bool,
    put_fn_body_on_new_line: bool,
    tab_size: usize,
    max_line_length: usize,
    put_fn_args_on_new_lines: bool,
    put_trailing_commas_on_let_bindings: bool,
    backup: bool,
}

impl FormatterConfigBuilder {
    pub fn put_fn_params_on_new_lines(
        self,
        put_fn_params_on_new_lines: bool,
    ) -> Self {
        Self {
            put_fn_params_on_new_lines,
            ..self
        }
    }

    pub fn use_set_notation_for_types(
        self,
        use_set_notation_for_types: bool,
    ) -> Self {
        Self {
            use_set_notation_for_types,
            ..self
        }
    }

    pub fn join_comments(
        self,
        join_comments: bool,
    ) -> Self {
        Self { join_comments, ..self }
    }

    pub fn newlines_between_items(
        self,
        newlines_between_items: usize,
    ) -> Self {
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

    pub fn put_variants_on_new_lines(
        self,
        put_variants_on_new_lines: bool,
    ) -> Self {
        Self {
            put_variants_on_new_lines,
            ..self
        }
    }

    pub fn put_list_elements_on_new_lines(
        self,
        put_list_elements_on_new_lines: bool,
    ) -> Self {
        Self {
            put_list_elements_on_new_lines,
            ..self
        }
    }

    pub fn put_fn_body_on_new_line(
        self,
        put_fn_body_on_new_line: bool,
    ) -> Self {
        Self {
            put_fn_body_on_new_line,
            ..self
        }
    }

    pub fn tab_size(
        self,
        tab_size: usize,
    ) -> Self {
        Self { tab_size, ..self }
    }

    pub fn max_line_length(
        self,
        max_line_length: usize,
    ) -> Self {
        Self { max_line_length, ..self }
    }

    pub fn put_fn_args_on_new_lines(
        self,
        put_fn_args_on_new_lines: bool,
    ) -> Self {
        Self {
            put_fn_args_on_new_lines,
            ..self
        }
    }

    pub fn put_trailing_commas_on_let_bindings(
        self,
        put_trailing_commas_on_let_bindings: bool,
    ) -> Self {
        Self {
            put_trailing_commas_on_let_bindings,
            ..self
        }
    }

    pub fn backup(
        self,
        backup: bool,
    ) -> Self {
        Self { backup, ..self }
    }

    pub fn build(self) -> FormatterConfig {
        FormatterConfig {
            put_fn_params_on_new_lines: self.put_fn_params_on_new_lines,
            use_set_notation_for_types: self.use_set_notation_for_types,
            join_comments: self.join_comments,
            newlines_between_items: self.newlines_between_items,
            newlines_between_comment_and_item: self.newlines_between_comment_and_item,
            put_variants_on_new_lines: self.put_variants_on_new_lines,
            put_list_elements_on_new_lines: self.put_list_elements_on_new_lines,
            put_fn_body_on_new_line: self.put_fn_body_on_new_line,
            tab_size: self.tab_size,
            max_line_length: self.max_line_length,
            put_fn_args_on_new_lines: self.put_fn_args_on_new_lines,
            put_trailing_commas_on_let_bindings: self.put_trailing_commas_on_let_bindings,
            backup: self.backup,
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
            put_list_elements_on_new_lines: false,
            put_fn_body_on_new_line: true,
            tab_size: 2,
            max_line_length: 80,
            put_fn_args_on_new_lines: false,
            put_trailing_commas_on_let_bindings: false,
            backup: false,
        }
    }
}
