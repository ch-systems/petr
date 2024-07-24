//! Top-level API for the petr programming language.
//! Exposes relevant APIs from all compiler stages and tooling.

#[cfg(not(feature = "no_std"))]
use std::{
    fs,
    path::{Path, PathBuf},
};

pub use petr_fmt::{format_sources, Formattable, FormatterConfig, FormatterContext};
pub use petr_ir::{Lowerer, LoweringError};
pub use petr_parse::Parser;
#[cfg(not(feature = "no_std"))]
pub use petr_pkg::{manifest::find_manifest, BuildPlan};
pub use petr_resolve::{resolve_symbols, Dependency};
pub use petr_typecheck::type_check;
pub use petr_utils::{render_error, Identifier, IndexMap, SourceId, SpannedItem};
pub use petr_vm::Vm;
#[cfg(not(feature = "no_std"))]
use termcolor::{ColorChoice, ColorSpec, StandardStream, WriteColor};

pub mod error {
    use petr_ir::LoweringError;
    use petr_utils::SpannedItem;
    use thiserror::Error;
    #[derive(Error, Debug)]
    pub enum PeteError {
        #[error(transparent)]
        Io(#[from] std::io::Error),
        #[error(transparent)]
        TomlSeriatlize(#[from] toml::ser::Error),
        #[cfg(not(feature = "no_std"))]
        #[error(transparent)]
        Pkg(#[from] petr_pkg::error::PkgError),
        #[error("Failed to lower code")]
        FailedToLower(#[from] SpannedItem<LoweringError>),
    }
}

#[cfg(not(feature = "no_std"))]
#[allow(clippy::type_complexity)]
pub fn load_project_and_dependencies(path: &Path) -> Result<(petr_pkg::Lockfile, Vec<(PathBuf, String)>, BuildPlan), crate::error::PeteError> {
    let manifest = petr_pkg::manifest::find_manifest(Some(path.to_path_buf())).expect("Failed to find manifest");
    let dependencies = manifest.dependencies;
    let mut stdout = StandardStream::stdout(ColorChoice::Always);

    if !dependencies.is_empty() {
        stdout.set_color(ColorSpec::new().set_bold(true))?;
        /*
        todo!(
            "instead of saying fetching, pay attention to if it already exists
        and print if it does or doesn't. also, check if checksum agrees with lockfile
        and use rev etc on github dep to determine thet key"
        );
        */
        println!(
            "Fetching {} {} for package {}",
            dependencies.len(),
            if dependencies.len() == 1 { "dependency" } else { "dependencies" },
            manifest.name
        );

        stdout.set_color(ColorSpec::new().set_bold(false))?;
    }
    let (lockfile, build_plan) = petr_pkg::load_dependencies(dependencies)?;

    let files = load_files(path);
    Ok((lockfile, files, build_plan))
}

#[cfg(not(feature = "no_std"))]
pub fn load_files(path: &Path) -> Vec<(PathBuf, String)> {
    let mut buf = Vec::new();

    fn read_petr_files(
        dir: &PathBuf,
        buf: &mut Vec<(PathBuf, String)>,
    ) {
        let entries = fs::read_dir(dir).expect("Failed to read directory");
        for entry in entries {
            let entry = entry.expect("Failed to read directory entry");
            let path = entry.path();
            if path.is_dir() {
                read_petr_files(&path, buf);
            } else if path.extension().and_then(|s| s.to_str()) == Some("pt") {
                let source = fs::read_to_string(&path).expect("Failed to read file");
                buf.push((path, source));
            }
        }
    }

    read_petr_files(&path.join("src"), &mut buf);
    buf
}

pub fn render_errors<T>(
    errs: Vec<SpannedItem<T>>,
    sources: &IndexMap<SourceId, (&'static str, &'static str)>,
) where
    T: miette::Diagnostic + Send + Sync + 'static,
{
    for err in errs {
        let rendered = petr_utils::render_error(sources, err);
        eprintln!("{:?}", rendered);
    }
}

#[cfg(not(feature = "no_std"))]
pub fn compile(
    path: PathBuf,
    timings: &mut petr_profiling::Timings,
) -> Result<Lowerer, crate::error::PeteError> {
    timings.start("full compile");
    timings.start("load project and dependencies");
    let (lockfile, buf, build_plan) = load_project_and_dependencies(&path)?;
    let lockfile_toml = toml::to_string(&lockfile).expect("Failed to serialize lockfile to TOML");
    let lockfile_path = path.join("petr.lock");
    fs::write(lockfile_path, lockfile_toml).expect("Failed to write lockfile");
    timings.end("load project and dependencies");

    // convert pathbufs into strings for the parser
    let buf = buf
        .into_iter()
        .map(|(pathbuf, s)| (pathbuf.to_string_lossy().to_string(), s))
        .collect::<Vec<_>>();

    timings.start("parsing stage");
    timings.start("parse user code");
    // parse
    // construct an interner for symbols, which will be used throughout the whole compilation.
    let parser = Parser::new(buf);
    let (ast, mut parse_errs, mut interner, mut source_map) = parser.into_result();

    timings.end("parse user code");
    timings.start("parse dependencies");

    let mut dependencies = Vec::with_capacity(build_plan.items.len());

    for item in build_plan.items {
        let (lockfile, buf, _build_plan) = load_project_and_dependencies(&item.path_to_source)?;
        // TODO(alex) -- transitive dependencies, get these build plans too

        let lockfile_toml = toml::to_string(&lockfile)?;
        let lockfile_path = path.join("petr.lock");
        fs::write(lockfile_path, lockfile_toml)?;
        // the idea here is that we re-use the interner and source map,
        // so we don't have to worry about scoping symbol IDs and source IDs to packages
        let parser = Parser::new_with_existing_interner_and_source_map(
            buf.into_iter()
                .map(|(pathbuf, s)| (pathbuf.to_string_lossy().to_string(), s))
                .collect::<Vec<_>>(),
            interner,
            source_map,
        );
        let (ast, mut new_parse_errs, new_interner, new_source_map) = parser.into_result();
        interner = new_interner;
        parse_errs.append(&mut new_parse_errs);
        source_map = new_source_map;
        dependencies.push(petr_resolve::Dependency {
            key: item.key,
            name: item.manifest.name,
            dependencies: item.depends_on,
            ast,
        });
    }

    timings.end("parse dependencies");
    timings.end("parsing stage");

    render_errors(parse_errs, &source_map);
    // errs.append(&mut parse_errs);
    // resolve symbols
    timings.start("symbol resolution");
    let (resolution_errs, resolved) = petr_resolve::resolve_symbols(ast, interner, dependencies);
    timings.end("symbol resolution");

    // TODO impl diagnostic for resolution errors
    if !resolution_errs.is_empty() {
        dbg!(&resolution_errs);
    }
    // errs.append(&mut resolution_errs);

    timings.start("type check");
    // type check
    let (type_errs, type_checker) = petr_typecheck::type_check(resolved);

    timings.end("type check");

    // TODO impl diagnostic for type errors
    if !type_errs.is_empty() {
        dbg!(&type_errs);
    }
    // errs.append(&mut type_errs);

    timings.start("lowering");
    let lowerer = Lowerer::new(type_checker);
    timings.end("lowering");

    Ok(lowerer?)
}
