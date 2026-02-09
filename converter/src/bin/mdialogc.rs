use markdialog_converter::{
    error::{Exit, handle_exit},
    parse::parse,
    special_argument, special_flag,
};
use std::{path::PathBuf, str::FromStr};

fn process() -> Result<(), Exit> {
    let mut args: Vec<String> = std::env::args().skip(1).collect();

    let help = special_flag!(args, "--help") || special_flag!(args, "-h");
    let version = special_flag!(args, "--version") || special_flag!(args, "-v");

    if version {
        let version = include_str!("../../version.txt");
        println!("{}", version.trim());
        return Err(Exit::Code(0));
    }

    if help || args.len() < 1 {
        let usage = include_str!("../../usage.txt");
        println!("{}", usage.trim());
        return Err(Exit::Code(0));
    }

    let input_file = get_input_file(&mut args)?;
    let output_ir_file = get_output_ir_file(&mut args)?.unwrap_or_else(|| {
        let mut path = input_file.clone();
        if let Some(file_name) = path.file_name() {
            let mut new_name = std::ffi::OsString::new();
            new_name.push(file_name);
            // Change extension to .dialog
            path.set_extension("dialog");
        } else {
            path.set_file_name("ir.dialog");
        }
        path
    });

    parse(input_file, output_ir_file)?;

    Ok(())
}

fn get_input_file(args: &mut Vec<String>) -> Result<PathBuf, Exit> {
    let input = match special_argument!(args, "--input") {
        Some(i) => i,
        None => match special_argument!(args, "-i") {
            Some(i) => i,
            None => {
                eprintln!("Missing required input argument. Use --input or -i.");
                std::process::exit(2);
            }
        },
    };

    let input_file = PathBuf::from_str(&input).map_err(|_| {
        eprintln!("Invalid file path `{}`!", input);
        Exit::Code(2)
    })?;

    Ok(input_file)
}

fn get_output_ir_file(args: &mut Vec<String>) -> Result<Option<PathBuf>, Exit> {
    let input = match special_argument!(args, "--output") {
        Some(i) => Some(i),
        None => match special_argument!(args, "-o") {
            Some(i) => Some(i),
            None => None,
        },
    };

    match input {
        Some(i) => {
            let input_file = PathBuf::from_str(&i).map_err(|_| {
                eprintln!("Invalid file path `{}`!", i);
                return Exit::Code(2);
            })?;
            Ok(Some(input_file))
        }
        None => Ok(None),
    }
}

fn main() {
    // Init colored
    #[cfg(windows)]
    colored::control::set_virtual_terminal(true).unwrap();

    match process() {
        Ok(_) => {}
        Err(e) => handle_exit(e),
    }
}
