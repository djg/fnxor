extern crate fn_decl_parser;
extern crate argparse;
extern crate itertools;

mod generator;

use argparse::{ArgumentParser, StoreTrue, Store, StoreOption};
use std::error::Error;
use std::io;

use generator::*;

fn main() {
    let mut verbose = false;
    let mut prefix = "foo".to_string();
    let mut input_path: Option<String> = None;
    let mut output_path: Option<String> = None;
    {
        let mut ap = ArgumentParser::new();
        ap.set_description("Process rust fn declaration into static and dynamic library loading.");
        ap.refer(&mut verbose)
            .add_option(&["-v", "--verbose"], StoreTrue, "");
        ap.refer(&mut prefix)
            .add_option(&["--prefix"], Store, "Prefix for library name");
        ap.refer(&mut input_path)
            .add_option(&["--input"], StoreOption, "Path of input. Defaults to stdin");
        ap.refer(&mut output_path)
            .add_option(&["--output"], StoreOption, "Path of output. Defaults to stdout");
        ap.parse_args_or_exit();
    }


    let mut gen = Generator::new();
        gen.verbose(verbose)
        .prefix(prefix);
    
    if let Some(input_path) = input_path {
        gen.with_input(input_path);
    }

    if let Some(output_path) = output_path {
        gen.with_output(output_path);
    }

    match gen.build() {
        Err(why) => panic!("Failed to generate code: {}", why.description()),
        _ => {}
    }
}
