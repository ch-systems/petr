use std::{fs, path::PathBuf};

use clap::Parser as ClapParser;
use swim_ir::Lowerer;
use swim_parse::Parser;
use swim_pkg::BuildPlan;
use swim_utils::{IndexMap, SourceId, SpannedItem};
use swim_vm::Vm;

#[derive(ClapParser)]
#[command(version = "0.0", author = "Alex H <alex@alex-hansen.com>")]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(ClapParser)]
enum Commands {
    #[command(about = "Run the program on a target")]
    Run {
        #[arg(short, long, help = "Target to run on", value_parser = ["vm", "native"], default_value = "vm")]
        target: String,
        #[arg(
            long,
            help = "Path to the directory which contains the swim.toml manifest and src subdir",
            default_value = "."
        )]
        path:   PathBuf,
        #[arg(short = 'm', long, help = "Print the timings table")]
        time:   bool,
    },
    #[command(about = "Print the IR of the program to stdout")]
    Ir {
        #[arg(
            long,
            help = "Path to the directory which contains the swim.toml manifest and src subdir",
            default_value = "."
        )]
        path: PathBuf,
    },
    #[command(about = "Format all sources in the project")]
    Fmt {
        #[arg(
            long,
            help = "Path to the directory which contains the swim.toml manifest and src subdir",
            default_value = "."
        )]
        path: PathBuf,
        #[arg(short = 'm', long, help = "Print the timings table")]
        time: bool,
    },
    #[command(about = "List the project sources")]
    Ls {
        #[arg(
            long,
            help = "Path to the directory which contains the swim.toml manifest and src subdir",
            default_value = "."
        )]
        path: PathBuf,
    },
}

fn main() {
    let cli = Cli::parse();

    match cli.command {
        Commands::Run { target, path, time } => {
            let mut timings = swim_profiling::Timings::default();
            timings.start("full compile");
            timings.start("load project and dependencies");
            let (lockfile, buf, _build_plan) = load_project_and_dependencies(&path);
            let lockfile_toml = toml::to_string(&lockfile).expect("Failed to serialize lockfile to TOML");
            fs::write("swim.lock", lockfile_toml).expect("Failed to write lockfile");

            timings.end("load project and dependencies");

            // convert pathbufs into strings for the parser
            let buf = buf
                .into_iter()
                .map(|(pathbuf, s)| (pathbuf.to_string_lossy().to_string(), s))
                .collect::<Vec<_>>();

            timings.start("parse");
            // parse
            let parser = Parser::new(buf);
            let (ast, parse_errs, interner, source_map) = parser.into_result();
            timings.end("parse");

            render_errors(parse_errs, &source_map);
            // errs.append(&mut parse_errs);
            // resolve symbols
            timings.start("symbol resolution");
            let (resolution_errs, resolved) = swim_resolve::resolve_symbols(ast, interner);
            timings.end("symbol resolution");

            // TODO impl diagnostic for resolution errors
            if !resolution_errs.is_empty() {
                dbg!(&resolution_errs);
            }
            // errs.append(&mut resolution_errs);

            timings.start("type check");
            // type check
            let (type_errs, type_checker) = swim_typecheck::type_check(resolved);

            timings.end("type check");

            // TODO impl diagnostic for type errors
            if !type_errs.is_empty() {
                dbg!(&type_errs);
            }
            // errs.append(&mut type_errs);

            timings.start("lowering");
            let lowerer: Lowerer = Lowerer::new(type_checker);
            timings.end("lowering");

            let (data, instructions) = lowerer.finalize();

            timings.start("execution");
            match target.to_lowercase().as_str() {
                "vm" => {
                    let mut vm = Vm::new(instructions, data);
                    vm.run().expect("Failed to run vm");
                },
                "native" => todo!(),
                _ => {
                    eprintln!("Invalid target: {}", target);
                },
            }
            timings.end("execution");
            timings.end("full compile");
            if time {
                println!("{}", timings.render());
            }
        },
        Commands::Fmt { path, time } => {
            let mut timings = swim_profiling::Timings::default();

            let manifest = swim_pkg::manifest::find_manifest(Some(path.clone())).expect("Failed to find manifest");

            timings.start("load files");
            let files = load_files(&path);
            timings.end("load files");

            timings.start("format");
            swim_fmt::format_sources(files, manifest.formatter.into()).expect("TODO errs");
            timings.end("format");

            if time {
                println!("{}", timings.render());
            }
        },
        Commands::Ls { path } => {
            let files = load_files(&path);
            for (path, _) in files {
                println!("{}", path.to_string_lossy());
            }
        },
        Commands::Ir { path } => {
            let (lockfile, buf, _build_plan) = load_project_and_dependencies(&path);
            let lockfile_toml = toml::to_string(&lockfile).expect("Failed to serialize lockfile to TOML");
            fs::write("swim.lock", lockfile_toml).expect("Failed to write lockfile");

            // convert pathbufs into strings for the parser
            let buf = buf
                .into_iter()
                .map(|(pathbuf, s)| (pathbuf.to_string_lossy().to_string(), s))
                .collect::<Vec<_>>();

            // parse
            let parser = Parser::new(buf);
            let (ast, parse_errs, interner, source_map) = parser.into_result();

            render_errors(parse_errs, &source_map);
            // errs.append(&mut parse_errs);
            // resolve symbols
            let (resolution_errs, resolved) = swim_resolve::resolve_symbols(ast, interner);

            // TODO impl diagnostic for resolution errors
            if !resolution_errs.is_empty() {
                dbg!(&resolution_errs);
            }
            // errs.append(&mut resolution_errs);

            // type check
            let (type_errs, type_checker) = swim_typecheck::type_check(resolved);

            // TODO impl diagnostic for type errors
            if !type_errs.is_empty() {
                dbg!(&type_errs);
            }
            // errs.append(&mut type_errs);
            let lowerer: Lowerer = Lowerer::new(type_checker);

            println!("{}", lowerer.pretty_print());
        },
    }
}

fn load_project_and_dependencies(path: &PathBuf) -> (swim_pkg::Lockfile, Vec<(PathBuf, String)>, BuildPlan) {
    let manifest = swim_pkg::manifest::find_manifest(Some(path.clone())).expect("Failed to find manifest");
    let dependencies = manifest.dependencies;
    let (lockfile, build_plan) = swim_pkg::load_dependencies(dependencies);

    let files = load_files(path);
    (lockfile, files, build_plan)
}

fn load_files(path: &PathBuf) -> Vec<(PathBuf, String)> {
    let mut buf = Vec::new();

    fn read_swim_files(
        dir: &PathBuf,
        buf: &mut Vec<(PathBuf, String)>,
    ) {
        let entries = fs::read_dir(dir).expect("Failed to read directory");
        for entry in entries {
            let entry = entry.expect("Failed to read directory entry");
            let path = entry.path();
            if path.is_dir() {
                read_swim_files(&path, buf);
            } else if path.extension().and_then(|s| s.to_str()) == Some("swim") {
                let source = fs::read_to_string(&path).expect("Failed to read file");
                buf.push((path, source));
            }
        }
    }

    read_swim_files(&path.join("src"), &mut buf);
    buf
}

fn render_errors<T>(
    errs: Vec<SpannedItem<T>>,
    sources: &IndexMap<SourceId, (&'static str, &'static str)>,
) where
    T: miette::Diagnostic + Send + Sync + 'static,
{
    for err in errs {
        let rendered = swim_utils::render_error(sources, err);
        eprintln!("{:?}", rendered);
    }
}
