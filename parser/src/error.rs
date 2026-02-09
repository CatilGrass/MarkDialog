use std::{i64, process::exit};

use colored::Colorize;
use unicode_width::UnicodeWidthStr;

#[derive(Debug)]
pub enum Exit {
    Code(i32),
    IoError(std::io::Error),
    SyntaxError {
        content: String,
        reason: String,
        line: i64,
        begin: i64,
        end: i64,
    },
}

impl From<std::io::Error> for Exit {
    fn from(error: std::io::Error) -> Self {
        Exit::IoError(error)
    }
}

pub fn handle_exit(e: Exit) {
    match e {
        Exit::Code(code) => exit(code),
        Exit::IoError(error) => print_parse_error(error.to_string()),
        Exit::SyntaxError {
            content,
            reason,
            line,
            begin,
            end,
        } => {
            print_syntax_error(content, reason, line, begin, end);
        }
    }
}

fn print_parse_error(content: impl AsRef<str>) {
    eprintln!("Parse Error !");
    eprintln!("{}", content.as_ref().trim());
    exit(1);
}

macro_rules! line {
    ($line:expr, $N:expr) => {
        if $line + $N <= 0 {
            " ".to_string()
        } else {
            ($line + $N).to_string()
        }
    };
}

pub fn print_syntax_error(content: String, reason: String, line: i64, begin: i64, end: i64) {
    let content_len = content.width() as i64;
    let end = end.clamp(begin, content_len);

    eprintln!("{}", "Parse Failed: Syntax Error".bright_yellow());
    eprintln!("{}{}", line!(line, -1), "|");

    let before: String = content.chars().take(begin.max(0) as usize).collect();
    let highlight_len = (end - begin).max(1) as usize;
    let highlight: String = content
        .chars()
        .skip(begin.max(0) as usize)
        .take(highlight_len)
        .collect();
    let after: String = content
        .chars()
        .skip((begin.max(0) + highlight_len as i64) as usize)
        .collect();

    eprintln!(
        "{}{} {}{}{}",
        line.to_string().cyan(),
        "|".cyan(),
        before.cyan(),
        highlight.bright_cyan(),
        after.cyan()
    );

    let prefix_chars: String = content.chars().take(begin.max(0) as usize).collect();
    let prefix_width = prefix_chars.width() as usize;

    eprintln!(
        "{}{} {}",
        line!(line, 1),
        "|",
        format!(
            "{}{}____ {}",
            " ".repeat(prefix_width),
            "^".repeat(((end - begin).max(1)) as usize),
            reason
        )
        .bright_cyan()
    );
    eprintln!("{}{}", line!(line, 2), "|");
    eprintln!(
        "{}",
        "Please fix the issue and run the program again".bright_yellow()
    );
    exit(1);
}
