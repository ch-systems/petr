use std::{fs, path::PathBuf, time::Instant};

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
    #[command(about = "run the program")]
    Run {
        #[arg(short, long, help = "target to run on", value_parser = ["vm", "native"], default_value = "vm")]
        target:   String,
        #[arg(short = 'i', long, help = "print the IR to stdout")]
        print_ir: bool,
        #[arg(
            long,
            help = "path to the directory which contains the swim.toml manifest and src subdir",
            default_value = "."
        )]
        path:     PathBuf,
        #[arg(long, help = "print the time it took to execute")]
        time:     bool,
    },
    #[command(about = "format all sources in the project")]
    Fmt {
        #[arg(
            long,
            help = "path to the directory which contains the swim.toml manifest and src subdir",
            default_value = "."
        )]
        path: PathBuf,
        #[arg(long, help = "print the time it took to execute")]
        time: bool,
    },
}

fn main() {
    let cli = Cli::parse();

    match cli.command {
        Commands::Run {
            target,
            print_ir,
            path,
            time,
        } => {
            let start = Instant::now();
            let (lockfile, buf, _build_plan) = load_project_and_dependencies(&path);
            let lockfile_toml = toml::to_string(&lockfile).expect("Failed to serialize lockfile to TOML");
            fs::write("swim.lock", lockfile_toml).expect("Failed to write lockfile");
            if time {
                let duration = start.elapsed();
                println!("loading project and dependencies time: {:?}", duration);
            }

            // convert pathbufs into strings for the parser
            let buf = buf
                .into_iter()
                .map(|(pathbuf, s)| (pathbuf.to_string_lossy().to_string(), s))
                .collect::<Vec<_>>();

            let parse_start = Instant::now();
            // parse
            let parser = Parser::new(buf);
            let (ast, parse_errs, interner, source_map) = parser.into_result();
            if time {
                let duration = parse_start.elapsed();
                println!("name binding time: {:?}", duration);
            }
            render_errors(parse_errs, &source_map);
            // errs.append(&mut parse_errs);
            // resolve symbols
            let resolution_start = Instant::now();
            let (resolution_errs, resolved) = swim_resolve::resolve_symbols(ast, interner);
            if time {
                let duration = resolution_start.elapsed();
                println!("name resolution time: {:?}", duration);
            }
            // TODO impl diagnostic for resolution errors
            if !resolution_errs.is_empty() {
                dbg!(&resolution_errs);
            }
            // errs.append(&mut resolution_errs);

            let typecheck_start = Instant::now();
            // type check
            let (type_errs, type_checker) = swim_typecheck::type_check(resolved);

            if time {
                let duration = typecheck_start.elapsed();
                println!("typechecking time: {:?}", duration);
            }
            // TODO impl diagnostic for type errors
            if !type_errs.is_empty() {
                dbg!(&type_errs);
            }
            // errs.append(&mut type_errs);

            let lowerer_start = Instant::now();
            let lowerer: Lowerer = Lowerer::new(type_checker);
            if time {
                let duration = lowerer_start.elapsed();
                println!("lowering to IR time: {:?}", duration);
            }

            if print_ir {
                println!("{}", lowerer.pretty_print());
            }
            let (data, instructions) = lowerer.finalize();

            let execution_start = Instant::now();
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

            if time {
                let duration = execution_start.elapsed();
                println!("Target execution time: {:?}", duration);

                let total_duration = start.elapsed();
                println!("Total execution time: {:?}", total_duration);
            }
        },
        Commands::Fmt { path, time } => {
            let start = Instant::now();

            let manifest = swim_pkg::manifest::find_manifest(Some(path.clone())).expect("Failed to find manifest");
            let files = load_files(&path);
            swim_fmt::format_sources(files, manifest.formatter.into()).expect("TODO errs");

            if time {
                let duration = start.elapsed();
                println!("Execution time: {:?}", duration);
            }
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
    let src_path = path.join("src");
    let files = fs::read_dir(&src_path)
        .expect("Failed to read src directory")
        .filter_map(|entry| {
            let entry = entry.expect("Failed to read directory entry");
            if entry.path().extension().and_then(|s| s.to_str()) == Some("swim") {
                Some(entry.path())
            } else {
                None
            }
        })
        .collect::<Vec<_>>();

    let mut buf = Vec::with_capacity(files.len());
    for file in files {
        let source = fs::read_to_string(&file).expect("Failed to read file");
        buf.push((file, source));
    }
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
