use serde::{Deserialize, Serialize};
use swim_pkg::{Dependency, GitDependency};

#[derive(Debug, Serialize, Deserialize)]
pub struct Manifest {
    pub author:       Option<String>,
    pub license:      Option<String>,
    #[serde(default)]
    pub formatter:    swim_fmt::FormatterConfig,
    #[serde(default)]
    pub dependencies: BTreeMap<String, Dependency>,
}

// check the current folder, then recursively upwards until a swim manfiest is found
use std::{
    collections::BTreeMap,
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

#[test]
fn what_is_my_manifest_format() {
    let manifest = Manifest {
        author:       Some("Alex Hansen <alex@alex-hansen.com>".into()),
        license:      Some("MIT".into()),
        formatter:    Default::default(),
        dependencies: BTreeMap::from_iter(vec![(
            "std".to_string(),
            Dependency::Git(GitDependency {
                git:    "https://github.com/sezna/swim-std".into(),
                branch: None,
                tag:    None,
                rev:    None,
            }),
        )]),
    };

    let manifest_str = toml::to_string(&manifest).unwrap();
    println!("manifest is: {}", manifest_str);
    panic!()
}
