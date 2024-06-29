use serde::{Deserialize, Serialize};

use crate::Dependency;

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct Manifest {
    pub author:       Option<String>,
    pub license:      Option<String>,
    pub name:         String,
    #[serde(default)]
    pub formatter:    FormatterConfigManifestFormat,
    #[serde(default)]
    pub dependencies: BTreeMap<String, Dependency>,
}

#[derive(Debug, Serialize, Deserialize, Clone, Default)]
pub struct FormatterConfigManifestFormat {
    pub put_fn_params_on_new_lines: Option<bool>,
    pub use_set_notation_for_types: Option<bool>,
    pub join_comments: Option<bool>,
    pub newlines_between_items: Option<usize>,
    pub newlines_between_comment_and_item: Option<usize>,
    pub put_variants_on_new_lines: Option<bool>,
    pub put_list_elements_on_new_lines: Option<bool>,
    pub put_fn_body_on_new_line: Option<bool>,
    pub tab_size: Option<usize>,
    pub max_line_length: Option<usize>,
    pub put_fn_args_on_new_lines: Option<bool>,
    pub put_trailing_commas_on_let_bindings: Option<bool>,
    pub backup: Option<bool>,
}

// TODO: this kind of sucks that we have to manually convert this to get a good serde serialization.
// It would be nice to get a compile error if we add something to FormatterConfig but forget it here.
impl From<FormatterConfigManifestFormat> for petr_fmt::FormatterConfig {
    fn from(value: FormatterConfigManifestFormat) -> Self {
        let mut builder = petr_fmt::FormatterConfigBuilder::default();
        if let Some(conf) = value.put_fn_params_on_new_lines {
            builder = builder.put_fn_params_on_new_lines(conf);
        };
        if let Some(conf) = value.use_set_notation_for_types {
            builder = builder.use_set_notation_for_types(conf);
        };
        if let Some(conf) = value.join_comments {
            builder = builder.join_comments(conf);
        };
        if let Some(conf) = value.newlines_between_items {
            builder = builder.newlines_between_items(conf);
        };
        if let Some(conf) = value.newlines_between_comment_and_item {
            builder = builder.newlines_between_comment_and_item(conf);
        };
        if let Some(conf) = value.put_variants_on_new_lines {
            builder = builder.put_variants_on_new_lines(conf);
        };
        if let Some(conf) = value.put_list_elements_on_new_lines {
            builder = builder.put_list_elements_on_new_lines(conf);
        };
        if let Some(conf) = value.put_fn_body_on_new_line {
            builder = builder.put_fn_body_on_new_line(conf);
        };
        if let Some(conf) = value.tab_size {
            builder = builder.tab_size(conf);
        };
        if let Some(conf) = value.max_line_length {
            builder = builder.max_line_length(conf);
        };
        if let Some(conf) = value.put_fn_args_on_new_lines {
            builder = builder.put_fn_args_on_new_lines(conf);
        };
        if let Some(conf) = value.put_trailing_commas_on_let_bindings {
            builder = builder.put_trailing_commas_on_let_bindings(conf);
        };
        if let Some(conf) = value.backup {
            builder = builder.backup(conf);
        };

        builder.build()
    }
}

// check the current folder, then recursively upwards until a petr manfiest is found
use std::{
    collections::BTreeMap,
    fs,
    path::{Path, PathBuf},
};

pub fn find_manifest(path: Option<PathBuf>) -> Result<Manifest, Box<dyn std::error::Error>> {
    fn search_dir(path: &Path) -> Option<PathBuf> {
        let manifest_path = path.join("pete.toml");
        if manifest_path.exists() {
            return Some(manifest_path);
        }
        path.parent().and_then(search_dir)
    }

    let start_path = path.unwrap_or_else(|| std::env::current_dir().unwrap());
    let manifest_path = search_dir(&start_path).ok_or("Manifest file not found")?;
    let manifest_content = fs::read_to_string(manifest_path)?;
    let manifest = toml::from_str(&manifest_content)?;
    Ok(manifest)
}
