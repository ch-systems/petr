//! This crate contains all of the logic for reasoning
//! about dependencies, pulling them in, creating lockfiles,
//! etc.

// TODO revisit this
#![allow(dead_code)]
pub mod manifest;

use manifest::Manifest;
use serde::{Deserialize, Serialize};

#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(untagged)]
pub enum Dependency {
    Git(GitDependency),
    Path(PathDependency),
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Lockfile {
    entries: Vec<LockfileEntry>,
}
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct LockfileEntry {
    name:       String,
    hash:       String,
    depends_on: BTreeMap<String, Dependency>,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct GitDependency {
    pub git:    String,
    pub branch: Option<String>,
    pub tag:    Option<String>,
    pub rev:    Option<String>,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct PathDependency {
    pub path: String,
}

use std::{
    collections::BTreeMap,
    fs,
    path::{Path, PathBuf},
};

/// An ordered list of dependencies to build in order.
#[derive(Debug)]
pub struct BuildPlan {
    items: Vec<BuildableItem>,
}

#[derive(Debug)]
pub struct BuildableItem {
    path_to_source: PathBuf,
    depends_on: Vec<DependencyKey>,
    key: DependencyKey,
}

pub type DependencyKey = String;

pub fn load_dependencies(deps: BTreeMap<String, Dependency>) -> (Lockfile, BuildPlan) {
    let mut entries = Vec::new();
    let mut files = Vec::new();

    for (_dep_name, dep_source) in deps {
        let dep = match dep_source {
            Dependency::Git(ref git_dep) => load_git_dependency(git_dep),
            Dependency::Path(ref path_dep) => load_path_dependency(path_dep, path_dep.path.clone()),
        };

        entries.push(dep.lock.clone());
        files.push(dep);
    }

    let lockfile = Lockfile { entries };
    (lockfile, order_dependencies(files))
}

fn order_dependencies(deps: Vec<LoadDependencyResult>) -> BuildPlan {
    let mut graph: BTreeMap<DependencyKey, Vec<DependencyKey>> = BTreeMap::new();

    for dep in &deps {
        graph.entry(dep.key.clone()).or_insert(vec![]);
        for (_, top_level_dependency) in &dep.lock.depends_on {
            let key = match top_level_dependency {
                Dependency::Git(git_dep) => git_dep.git.clone(),
                Dependency::Path(path_dep) => path_dep.path.clone(),
            };
            graph.entry(dep.key.clone()).or_insert(vec![]).push(key);

            // Recursively include transient dependencies
            fn add_transient_dependencies(
                graph: &mut BTreeMap<DependencyKey, Vec<DependencyKey>>,
                dep_key: &DependencyKey,
                dependency: &Dependency,
                deps: &[LoadDependencyResult],
            ) {
                let key = match dependency {
                    Dependency::Git(git_dep) => git_dep.git.clone(),
                    Dependency::Path(path_dep) => path_dep.path.clone(),
                };
                graph.entry(dep_key.clone()).or_insert(vec![]).push(key.clone());

                if let Some(dep) = deps.iter().find(|d| d.key == key) {
                    for (_, trans_dependency) in &dep.lock.depends_on {
                        add_transient_dependencies(graph, &key, trans_dependency, deps);
                    }
                }
            }

            add_transient_dependencies(&mut graph, &dep.key, top_level_dependency, &deps[..]);
        }
    }

    for dep in &deps {
        for (_, dependency) in &dep.lock.depends_on {
            let dep_key = match dependency {
                Dependency::Git(git_dep) => git_dep.git.parse().unwrap(),
                Dependency::Path(path_dep) => path_dep.path.parse().unwrap(),
            };
            graph.entry(dep_key).or_insert(vec![]).push(dep.key.clone());
        }
    }

    let mut sorted_keys = Vec::new();
    let mut visited = BTreeMap::new();

    fn visit(
        node: DependencyKey,
        graph: &BTreeMap<DependencyKey, Vec<DependencyKey>>,
        visited: &mut BTreeMap<DependencyKey, bool>,
        sorted_keys: &mut Vec<DependencyKey>,
    ) {
        if let Some(&is_visited) = visited.get(&node) {
            if is_visited {
                return;
            }
        }

        visited.insert(node.clone(), true);

        if let Some(neighbors) = graph.get(&node) {
            for neighbor in neighbors.iter() {
                visit(neighbor.clone(), graph, visited, sorted_keys);
            }
        }

        sorted_keys.push(node);
    }

    for node in graph.keys() {
        visit(node.clone(), &graph, &mut visited, &mut sorted_keys);
    }

    // load all transient deps, and combine them with `deps`
    // then sort them by the order of the `sorted_keys`
    let mut all_deps = deps.clone();
    for dep in &deps {
        for (_, dependency) in &dep.lock.depends_on {
            let key = match dependency {
                Dependency::Git(git_dep) => git_dep.git.clone(),
                Dependency::Path(path_dep) => path_dep.path.clone(),
            };
            if !all_deps.iter().any(|d| d.key == key) {
                let transient_dep = match dependency {
                    Dependency::Git(git_dep) => load_git_dependency(git_dep),
                    Dependency::Path(path_dep) => load_path_dependency(path_dep, key.clone()),
                };
                all_deps.push(transient_dep);
            }
        }
    }

    let items = sorted_keys
        .into_iter()
        .map(|key| {
            let dep = all_deps.iter().find(|x| &x.key == &key).unwrap();
            let path = Path::new(&dep.lock.name);
            BuildableItem {
                path_to_source: path.to_path_buf(),
                depends_on: dep
                    .lock
                    .depends_on
                    .iter()
                    .map(|(_, dep)| match dep {
                        Dependency::Git(git_dep) => git_dep.git.clone(),
                        Dependency::Path(path_dep) => path_dep.path.clone(),
                    })
                    .collect(),
                key,
            }
        })
        .collect();

    BuildPlan { items }
}

#[derive(Clone)]
struct LoadDependencyResult {
    lock:     LockfileEntry,
    files:    Vec<(String, String)>,
    manifest: Manifest,
    // a unique identifier for this dependency,
    key:      String,
}

fn load_git_dependency(dep: &GitDependency) -> LoadDependencyResult {
    // determine an os-independent directory in `~/.petr` to store clones in. On windows this is `C:\Users\<user>\.petr`, on UNIX systems this is `~/.petr`
    // etc.
    // clone the git repository into the directory, then read all files in the directory (potentially using load_path_dependency)
    // calculate the LockfileEntry
    let home_dir = dirs::home_dir().expect("Failed to get home directory");
    let petr_dir = home_dir.join(".petr");
    if !petr_dir.exists() {
        fs::create_dir_all(&petr_dir).expect("Failed to create .petr directory");
    }

    let repo_dir = petr_dir.join(&dep.git.replace("/", "_"));

    if !repo_dir.exists() {
        let _ = git2::Repository::clone(&dep.git, &repo_dir).expect("Failed to clone Git repository");
    }

    let path_dep = PathDependency {
        path: repo_dir.to_string_lossy().into_owned(),
    };

    load_path_dependency(&path_dep, dep.git.clone())
}

fn load_path_dependency(
    dep: &PathDependency,
    key: DependencyKey,
) -> LoadDependencyResult {
    let path = Path::new(&dep.path).join("src");
    let entries = fs::read_dir(&path).unwrap_or_else(|_| panic!("Failed to read directory: {}", dep.path));

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

    // TODO(alex) canonicalize path dependencies so they are relative to the pete.toml file
    let petr_toml_path = Path::new(&dep.path).join("pete.toml");
    let manifest_content = fs::read_to_string(&petr_toml_path).expect("Failed to read pete.toml");
    let manifest: Manifest = toml::from_str(&manifest_content).expect("Failed to parse pete.toml");

    let lockfile_entry = LockfileEntry {
        name:       dep.path.clone(),
        hash:       calculate_lockfile_hash(files.clone()),
        depends_on: manifest.dependencies.clone(),
    };

    LoadDependencyResult {
        lock: lockfile_entry,
        files,
        manifest,
        key,
    }
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
