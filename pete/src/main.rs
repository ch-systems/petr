use std::path::PathBuf;

use clap::Parser as ClapParser;
use petr_api::*;

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
            help = "Path to the directory which contains the pete.toml manifest and src subdir",
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
            help = "Path to the directory which contains the pete.toml manifest and src subdir",
            default_value = "."
        )]
        path: PathBuf,
    },
    #[command(about = "Format all sources in the project")]
    Fmt {
        #[arg(
            long,
            help = "Path to the directory which contains the pete.toml manifest and src subdir",
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
            help = "Path to the directory which contains the pete.toml manifest and src subdir",
            default_value = "."
        )]
        path: PathBuf,
    },
}

fn main() -> Result<(), error::PeteError> {
    let cli = Cli::parse();

    match cli.command {
        Commands::Run { target, path, time } => {
            let mut timings = petr_profiling::Timings::default();
            let lowerer = petr_api::compile(path, &mut timings)?;

            let (data, instructions) = lowerer.finalize();

            timings.end("full compile");

            timings.start("execution");
            match target.to_lowercase().as_str() {
                "vm" => {
                    let vm = Vm::new(instructions, data);
                    let result = vm.run().expect("Failed to run vm");
                    println!("VM terminated with stack:\n{:#?}", result);
                },
                "native" => todo!(),
                _ => {
                    eprintln!("Invalid target: {}", target);
                },
            }
            timings.end("execution");
            if time {
                println!("{}", timings.render());
            }
        },
        Commands::Fmt { path, time } => {
            let mut timings = petr_profiling::Timings::default();

            let manifest = petr_api::find_manifest(Some(path.clone())).expect("Failed to find manifest");

            timings.start("load files");
            let files = petr_api::load_files(&path);
            timings.end("load files");

            timings.start("format");
            format_sources(files, manifest.formatter.into())?;
            timings.end("format");

            if time {
                println!("{}", timings.render());
            }
        },
        Commands::Ls { path } => {
            let files = petr_api::load_files(&path);
            for (path, _) in files {
                println!("{}", path.to_string_lossy());
            }
        },
        Commands::Ir { path } => {
            let lowerer = petr_api::compile(path, &mut petr_profiling::Timings::default())?;

            println!("{}", lowerer.pretty_print());
        },
    }
    Ok(())
}
