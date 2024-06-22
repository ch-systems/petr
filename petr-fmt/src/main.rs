use std::{
    fs,
    io::{self},
    path::PathBuf,
};

use petr_fmt::{format_sources, FormatterConfigBuilder};
use structopt::StructOpt;

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
    let config = FormatterConfigBuilder::default().backup(opt.backup).build();

    format_sources(sources, config)
}
