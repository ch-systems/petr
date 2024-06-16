use serde::{Deserialize, Serialize};
use swim_pkg::Dependency;

#[derive(Debug, Serialize, Deserialize)]
pub struct Manifest {
    formatter:    swim_fmt::FormatterConfig,
    dependencies: Vec<Dependency>,
}

// check the current folder, then recursively upwards until a swim manfiest is found
use std::{
    fs,
    path::{Path, PathBuf},
};

pub fn find_manifest() -> Result<Manifest, Box<dyn std::error::Error>> {
    fn search_dir(path: &Path) -> Option<PathBuf> {
        let manifest_path = path.join("swim.toml");
        if manifest_path.exists() {
            return Some(manifest_path);
        }
        path.parent().and_then(search_dir)
    }

    let current_dir = std::env::current_dir()?;
    let manifest_path = search_dir(&current_dir).ok_or("Manifest file not found")?;
    let manifest_content = fs::read_to_string(manifest_path)?;
    let manifest = toml::from_str(&manifest_content)?;
    Ok(manifest)
}
