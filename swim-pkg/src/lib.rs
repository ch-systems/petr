//! This crate contains all of the logic for reasoning
//! about dependencies, pulling them in, creating lockfiles,
//! etc.

use serde::{Deserialize, Serialize};

#[derive(Debug, Serialize, Deserialize)]
#[serde(untagged)]
pub enum Dependency {
    Git(GitDependency),
    Path(PathDependency),
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Lockfile {
    entries: Vec<LockfileEntry>,
}
#[derive(Debug, Serialize, Deserialize)]
pub struct LockfileEntry {
    name:       String,
    hash:       String,
    depends_on: Vec<Dependency>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct GitDependency {
    pub git:    String,
    pub branch: Option<String>,
    pub tag:    Option<String>,
    pub rev:    Option<String>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct PathDependency {
    pub path: String,
}

use std::{collections::BTreeMap, fs, path::Path};

pub fn load_dependencies(deps: BTreeMap<String, Dependency>) -> (Lockfile, Vec<Vec<(String, String)>>) {
    let mut entries = Vec::new();
    let mut files = Vec::new();

    for (dep_name, dep_source) in deps {
        let (entry, sources) = match dep_source {
            Dependency::Git(ref git_dep) => load_git_dependency(git_dep),
            Dependency::Path(ref path_dep) => load_path_dependency(path_dep),
        };

        entries.push(entry);
        files.push(sources);
    }

    (Lockfile { entries }, files)
}

fn load_git_dependency(dep: &GitDependency) -> (LockfileEntry, Vec<(String, String)>) {
    // determine an os-independent directory in `~/.swim` to store clones in. On windows this is `C:\Users\<user>\.swim`, on UNIX systems this is `~/.swim`
    // etc.
    // clone the git repository into the directory, then read all files in the directory (potentially using load_path_dependency)
    // calculate the LockfileEntry
    let home_dir = dirs::home_dir().expect("Failed to get home directory");
    let swim_dir = home_dir.join(".swim");
    if !swim_dir.exists() {
        fs::create_dir_all(&swim_dir).expect("Failed to create .swim directory");
    }

    let repo_dir = swim_dir.join(&dep.git.replace("/", "_"));
    if !repo_dir.exists() {
        let _ = git2::Repository::clone(&dep.git, &repo_dir).expect("Failed to clone Git repository");
    }

    let path_dep = PathDependency {
        path: repo_dir.to_string_lossy().into_owned(),
    };

    load_path_dependency(&path_dep)
}

fn load_path_dependency(dep: &PathDependency) -> (LockfileEntry, Vec<(String, String)>) {
    let path = Path::new(&dep.path).join("src");
    let entries = fs::read_dir(path).unwrap_or_else(|_| panic!("Failed to read directory: {}", dep.path));

    let files: Vec<_> = entries
        .filter_map(|entry| {
            let entry = entry.ok()?;
            let path = entry.path();
            if path.is_file() {
                let file_name = path.file_name()?.to_string_lossy().into_owned();
                let content = fs::read_to_string(&path).ok()?;
                Some((file_name, content))
            } else {
                None
            }
        })
        .collect();

    let lockfile_entry = LockfileEntry {
        name:       dep.path.clone(),
        hash:       calculate_lockfile_hash(files.clone()),
        depends_on: vec![], // Placeholder for dependency resolution logic
    };

    (lockfile_entry, files)
}

fn calculate_lockfile_hash(sources: Vec<(String, String)>) -> String {
    // hash all sources and contents into a u128
    use bcrypt::{hash, DEFAULT_COST};

    let mut hasher = blake3::Hasher::new();
    for (name, content) in sources {
        hasher.update(name.as_bytes());
        hasher.update(content.as_bytes());
    }
    let hash_bytes = hasher.finalize();
    let hash_bytes = hash_bytes.as_bytes();
    let hash_string = hash(hash_bytes, DEFAULT_COST).expect("Failed to hash");
    u128::from_be_bytes(hash_string.as_bytes()[..16].try_into().expect("Failed to convert hash to u128")).to_string()
}
