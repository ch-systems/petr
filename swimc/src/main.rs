use clap::Parser as ClapParser;
use std::fs;
use swim_parse::Parser;
use swim_utils::PrettyPrint;

#[derive(ClapParser)]
#[command(version = "0.0", author = "Alex H <alex@alex-hansen.com>")]
struct Cli {
    #[arg(help = "sources to compile", required = true, num_args(1..))]
    files: Vec<String>,
}

fn main() {
    let cli = Cli::parse();

    let mut buf = Vec::with_capacity(cli.files.len());
    for file in cli.files {
        let source = fs::read_to_string(&file).expect("Failed to read file");
        buf.push((file, source));
    }
    let parser = Parser::new(buf);
    let (ast, errs, interner, source_map) = parser.into_result();
    match errs {
        errs if !errs.is_empty() => {
            for err in errs {
                let rendered = swim_utils::render_error(&source_map, err);
                eprintln!("{:?}", rendered);
            }
            return;
        }
        _ => {
            println!("{}", ast.pretty_print(&interner, 0));
            return;
        }
    }
}
