use std::{
    fs,
    io::{self, Write},
    path::PathBuf,
};

use structopt::StructOpt;
use swim_fmt::{ctx::FormatterContext, Formattable};
use swim_parse::Parser;
use swim_utils::render_error;

#[derive(StructOpt, Debug)]
#[structopt(name = "formatter")]
struct Opt {
    /// Input source files to format
    #[structopt(parse(from_os_str))]
    input: Vec<PathBuf>,

    /// Save backup files with .bak extension
    #[structopt(long)]
    backup: bool,
}

fn main() -> io::Result<()> {
    let opt = Opt::from_args();

    // read sources from disk
    let sources = opt.input.iter().map(fs::read_to_string).collect::<Result<Vec<_>, _>>()?;

    let sources = opt.input.into_iter().zip(sources).collect::<Vec<_>>();

    let longest_source_name = sources.iter().map(|(path, _)| path.display().to_string().len()).max().unwrap_or(0);

    let distance_to_check = longest_source_name + 5;

    for (source_name, source) in sources {
        let num_dots_to_display = distance_to_check - source_name.display().to_string().len();
        print!("formatting {}...", source_name.display());
        let string_source_name = source_name.to_string_lossy();
        let parser = Parser::new(vec![(string_source_name.clone(), source.clone())]);
        let (ast, errs, interner, source_map) = parser.into_result();
        print!("{}", ".".repeat(num_dots_to_display));
        if !errs.is_empty() {
            errs.into_iter().for_each(|err| eprintln!("{:?}", render_error(&source_map, err)));
            panic!("fmt failed: code didn't parse");
        }
        let mut ctx = FormatterContext::from_interner(interner).with_config(Default::default());
        let formatted_content = ast.line_length_aware_format(&mut ctx).render();
        // Create a new formatter context
        print!("...");

        if opt.backup {
            let backup_path = format!("{string_source_name}.bak");
            fs::write(backup_path, &source)?;
        }

        // Write the formatted content back to the file
        let mut file = fs::File::create(source_name)?;
        file.write_all(formatted_content.as_bytes())?;
        println!("✅");
    }

    Ok(())
}
