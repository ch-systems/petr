use std::fs;

use clap::Parser as ClapParser;
use swim_ir::Lowerer;
use swim_parse::Parser;
use swim_utils::{IndexMap, PrettyPrint, SourceId, SpannedItem};
use swim_vm::Vm;

#[derive(ClapParser)]
#[command(version = "0.0", author = "Alex H <alex@alex-hansen.com>")]
struct Cli {
    #[command(subcommand)]
    command:  Commands,
    #[arg(short = 'i', long, help = "print the IR to stdout")]
    print_ir: bool,
}

#[derive(ClapParser)]
enum Commands {
    #[command(about = "Run the program")]
    Run {
        #[arg(help = "sources to compile", required = true, num_args(1..))]
        files:  Vec<String>,
        #[arg(short, long, help = "target to run on", value_parser = ["vm", "native"])]
        target: String,
    },
}

fn main() {
    let cli = Cli::parse();

    match cli.command {
        Commands::Run { files, target } => {
            println!("Running on target: {}", target);
            let mut buf = Vec::with_capacity(files.len());
            for file in files {
                let source = fs::read_to_string(&file).expect("Failed to read file");
                buf.push((file, source));
            }
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

            let mut lowerer: Lowerer = Lowerer::new(type_checker);
            if cli.print_ir {
                println!("{}", lowerer.pretty_print());
            }

            let (data, instructions) = lowerer.finalize();

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
        },
    }
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
