pub struct FormatterConfig {
    put_fn_params_on_new_lines: bool,
    use_set_notation_for_types: bool,
    join_comments: bool,
}

impl FormatterConfig {
    pub fn put_fn_params_on_new_lines(&self) -> bool {
        self.put_fn_params_on_new_lines
    }

    pub fn use_set_notation_for_types(&self) -> bool {
        self.use_set_notation_for_types
    }

    pub fn join_comments(&self) -> bool {
        self.join_comments
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

    pub fn build(self) -> FormatterConfig {
        FormatterConfig {
            put_fn_params_on_new_lines: self.put_fn_params_on_new_lines,
            use_set_notation_for_types: self.use_set_notation_for_types,
            join_comments: self.join_comments,
        }
    }
}

impl Default for FormatterConfigBuilder {
    fn default() -> Self {
        Self {
            put_fn_params_on_new_lines: true,
            use_set_notation_for_types: true,
            join_comments: true,
        }
    }
}
