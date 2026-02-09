use proc_macro::TokenStream;
use sha2::Digest;
use std::fs;

use syn::parse::{Parse, ParseStream, Result};
use syn::{LitInt, LitStr, parse_macro_input};

struct StepInput {
    s: String,
    n: i64,
}

impl Parse for StepInput {
    fn parse(input: ParseStream) -> Result<Self> {
        let s = input.parse::<LitStr>()?.value();
        let n = if input.is_empty() {
            0
        } else {
            input.parse::<syn::Token![,]>()?;
            let lit = input.parse::<LitInt>()?;
            lit.base10_parse::<i64>()?
        };
        Ok(StepInput { s, n })
    }
}

struct MarkDialogInput {
    mod_name: syn::Ident,
    file_path: String,
}

impl Parse for MarkDialogInput {
    fn parse(input: ParseStream) -> Result<Self> {
        let mod_name = input.parse::<syn::Ident>()?;
        input.parse::<syn::Token![=]>()?;
        let file_path_lit = input.parse::<LitStr>()?;
        let file_path = file_path_lit.value();

        Ok(MarkDialogInput {
            mod_name,
            file_path,
        })
    }
}

/// Generates a StepId enum based on a string
/// Will select the StepId imported or declared in the current module
///
/// ```
/// # use res_gen_macros::step;
/// #[allow(non_camel_case_types)]
/// #[derive(Debug, PartialEq)]
/// enum StepId {
///     R_09B538D1_0, // Begin
/// }
///
/// assert_eq!(step!("Begin"), StepId::R_09B538D1_0);
/// ```
#[proc_macro]
pub fn step(input: TokenStream) -> TokenStream {
    let StepInput { s, n } = parse_macro_input!(input as StepInput);

    let hash = sha2::Sha256::digest(s.as_bytes());
    let hex = format!("{:x}", hash);
    let short = &hex[..8];

    let ident_str = format!("R_{}_{}", short.to_uppercase(), n);
    let ident = syn::Ident::new(&ident_str, proc_macro2::Span::call_site());

    let expanded = quote::quote! {
        StepId::#ident
    };

    expanded.into()
}

/// Generates a dialog module from a .dialog file
///
/// ```ignore
/// use markdialog::generate::{markdialog, step};
///
/// // Define here
/// markdialog!(my = "Chapter1.dialog");
///
/// fn main() {
///     let my_step = my::get_step(step!("Begin")).unwrap();
///     let sentence = my_step.sentences[0];
///
///     // Print my sentences
///     println!(
///         "{} said : \"{}\"",
///         sentence.character.unwrap_or_default(),
///         sentence
///             .content_tokens
///             .iter()
///             .map(|token| match token {
///                 my::Token::Text(t) => t,
///                 my::Token::BoldText(t) => t,
///                 my::Token::ItalicText(t) => t,
///                 my::Token::BoldItalicText(t) => t,
///                 _ => "",
///             }
///             .to_string())
///             .collect::<Vec<String>>()
///             .join("")
///     )
/// }
/// ```
#[proc_macro]
pub fn markdialog(input: TokenStream) -> TokenStream {
    let MarkDialogInput {
        mod_name,
        file_path,
    } = parse_macro_input!(input as MarkDialogInput);

    // Read file content
    let content = match fs::read_to_string(&file_path) {
        Ok(content) => content,
        Err(e) => {
            return syn::Error::new(
                proc_macro2::Span::call_site(),
                format!("Failed to read dialog file '{}': {}", file_path, e),
            )
            .to_compile_error()
            .into();
        }
    };

    // Parse dialog file
    let parsed_dialog = match parse_dialog_file(&content) {
        Ok(parsed) => parsed,
        Err(e) => {
            return syn::Error::new(
                proc_macro2::Span::call_site(),
                format!("Failed to parse dialog file: {}", e),
            )
            .to_compile_error()
            .into();
        }
    };

    // Generate code
    let expanded = generate_dialog_module(&mod_name, &parsed_dialog);

    expanded.into()
}

fn parse_dialog_file(content: &str) -> std::result::Result<DialogFile, String> {
    let mut steps = Vec::new();
    let mut current_step_id: Option<String> = None;
    let mut current_sentences: Vec<SentenceData> = Vec::new();

    for line in content.lines() {
        let line = line.trim();

        if line.is_empty() {
            continue;
        }

        // Check if it's a Step definition line
        if line.starts_with("@@@@@@@@@@") {
            // Save previous Step
            if let Some(step_id) = current_step_id.take() {
                steps.push(StepData {
                    id: step_id,
                    sentences: std::mem::take(&mut current_sentences),
                });
            }

            // Extract new Step ID
            let step_id = line["@@@@@@@@@@".len()..].trim().to_string();
            current_step_id = Some(step_id);
        } else if let Some(_step_id) = &current_step_id {
            // Parse Sentence line
            match parse_sentence_line(line) {
                Ok(sentence) => current_sentences.push(sentence),
                Err(e) => return Err(format!("Failed to parse sentence line '{}': {}", line, e)),
            }
        }
    }

    // Save the last Step
    if let Some(step_id) = current_step_id {
        steps.push(StepData {
            id: step_id,
            sentences: current_sentences,
        });
    }

    Ok(DialogFile { steps })
}

fn parse_sentence_line(line: &str) -> std::result::Result<SentenceData, String> {
    // Split character part and content part
    let parts: Vec<&str> = line.split("->").collect();
    if parts.len() != 2 {
        return Err(format!("Invalid sentence line format: {}", line));
    }

    let left_part = parts[0].trim();
    let right_part = parts[1].trim();

    // Parse character part
    let (character, silence_switch) = parse_character_part(left_part)?;

    // Parse content blocks
    let content_tokens = parse_content_blocks(left_part)?;

    // Parse jump block
    let next_step = parse_next_step(right_part)?;

    Ok(SentenceData {
        character,
        content_tokens,
        next_step,
        silence_switch,
    })
}

fn parse_character_part(line: &str) -> std::result::Result<(Option<String>, bool), String> {
    // Find the position of the first ']'
    let end_bracket = line
        .find(']')
        .ok_or_else(|| "No closing bracket found for character".to_string())?;
    let character_part = &line[..=end_bracket];

    if character_part == "[]" {
        return Ok((None, false));
    }

    if character_part == "[**]" {
        return Ok((None, true));
    }

    // Check if it's wrapped with *
    let has_stars = character_part.starts_with("[*") && character_part.ends_with("*]");

    let start = if has_stars { 2 } else { 1 };
    let end = character_part.len() - (if has_stars { 2 } else { 1 });

    let character_content = &character_part[start..end];

    // Handle Unicode escape sequences
    let decoded = decode_unicode_escapes(character_content)?;

    Ok((Some(decoded), has_stars))
}

fn parse_content_blocks(line: &str) -> std::result::Result<Vec<TokenData>, String> {
    let mut tokens = Vec::new();
    let mut current_pos;

    // Skip the character part
    let first_bracket = line
        .find(']')
        .ok_or_else(|| "No closing bracket found".to_string())?;
    current_pos = first_bracket + 1;

    while current_pos < line.len() {
        // Find the next '['
        if let Some(start) = line[current_pos..].find('[') {
            let start_pos = current_pos + start;

            // Find the matching ']'
            let mut bracket_count = 0;
            let mut end_pos = None;

            for (i, ch) in line[start_pos..].char_indices() {
                match ch {
                    '[' => bracket_count += 1,
                    ']' => {
                        bracket_count -= 1;
                        if bracket_count == 0 {
                            end_pos = Some(start_pos + i);
                            break;
                        }
                    }
                    _ => {}
                }
            }

            if let Some(end) = end_pos {
                let block = &line[start_pos..=end];

                // Parse content block
                if let Ok(token) = parse_content_block(block) {
                    tokens.push(token);
                }

                current_pos = end + 1;
            } else {
                break;
            }
        } else {
            break;
        }
    }

    Ok(tokens)
}

fn parse_content_block(block: &str) -> std::result::Result<TokenData, String> {
    // Format: [label:[content]]
    let inner_start = block
        .find(':')
        .ok_or_else(|| "No colon in content block".to_string())?;
    let label = &block[1..inner_start];

    // Find the start and end of content
    let content_start = inner_start + 2; // Skip ':['
    let content_end = block.len() - 2; // Skip the trailing ']]'

    if content_start >= content_end {
        return Err("Empty content in block".to_string());
    }

    let content = &block[content_start..content_end];

    // Handle Unicode escape sequences
    let decoded_content = decode_unicode_escapes(content)?;

    // Check for formatting markers
    let trimmed_content = decoded_content.trim_matches('`');

    match label {
        "text" => {
            // Check text formatting
            if trimmed_content.starts_with("**") && trimmed_content.ends_with("**") {
                let inner = &trimmed_content[2..trimmed_content.len() - 2];
                Ok(TokenData::BoldText(inner.to_string()))
            } else if trimmed_content.starts_with("*") && trimmed_content.ends_with("*") {
                let inner = &trimmed_content[1..trimmed_content.len() - 1];
                Ok(TokenData::ItalicText(inner.to_string()))
            } else if trimmed_content.starts_with("***") && trimmed_content.ends_with("***") {
                let inner = &trimmed_content[3..trimmed_content.len() - 3];
                Ok(TokenData::BoldItalicText(inner.to_string()))
            } else {
                Ok(TokenData::Text(trimmed_content.to_string()))
            }
        }
        "code" => Ok(TokenData::Code(trimmed_content.to_string())),
        _ => Err(format!("Unknown label: {}", label)),
    }
}

fn parse_next_step(block: &str) -> std::result::Result<Option<String>, String> {
    if block == "[]" {
        return Ok(None);
    }

    // 格式: [#step_id]
    if !block.starts_with("[#") || !block.ends_with(']') {
        return Err(format!("Invalid next step format: {}", block));
    }

    let step_id = &block[2..block.len() - 1];
    Ok(Some(step_id.to_string()))
}

fn decode_unicode_escapes(input: &str) -> std::result::Result<String, String> {
    let mut result = String::new();
    let mut chars = input.chars().peekable();

    while let Some(ch) = chars.next() {
        if ch == '\\' {
            if let Some(&next) = chars.peek() {
                if next == 'u' {
                    chars.next(); // skip 'u'

                    // read 4 hex digits
                    let mut hex_str = String::new();
                    for _ in 0..4 {
                        if let Some(&hex_char) = chars.peek() {
                            if hex_char.is_ascii_hexdigit() {
                                hex_str.push(hex_char);
                                chars.next();
                            } else {
                                return Err("Invalid Unicode escape sequence".to_string());
                            }
                        } else {
                            return Err("Incomplete Unicode escape sequence".to_string());
                        }
                    }

                    // parse hex number
                    if let Ok(code_point) = u32::from_str_radix(&hex_str, 16) {
                        if let Some(unicode_char) = char::from_u32(code_point) {
                            result.push(unicode_char);
                        } else {
                            return Err(format!("Invalid Unicode code point: {}", code_point));
                        }
                    } else {
                        return Err(format!("Invalid hex number: {}", hex_str));
                    }
                } else {
                    result.push(ch);
                    result.push(next);
                    chars.next();
                }
            } else {
                result.push(ch);
            }
        } else {
            result.push(ch);
        }
    }

    Ok(result)
}

fn generate_dialog_module(mod_name: &syn::Ident, dialog: &DialogFile) -> proc_macro2::TokenStream {
    // Generate StepId enum
    let step_id_variants: Vec<_> = dialog
        .steps
        .iter()
        .map(|step| {
            let id_upper = step.id.to_uppercase().replace('-', "_");
            let variant_name =
                syn::Ident::new(&format!("R_{}", id_upper), proc_macro2::Span::call_site());
            quote::quote! {
                #variant_name,
            }
        })
        .collect();

    let step_id_variants_formatted = if step_id_variants.is_empty() {
        quote::quote! {}
    } else {
        quote::quote! {
            #(#step_id_variants)*
        }
    };

    // Generate get_step function
    let match_arms: Vec<_> = dialog
        .steps
        .iter()
        .map(|step| {
            let id_upper = step.id.to_uppercase().replace('-', "_");
            let variant_name =
                syn::Ident::new(&format!("R_{}", id_upper), proc_macro2::Span::call_site());

            // Generate sentences
            let sentences: Vec<_> = step
                .sentences
                .iter()
                .map(|sentence| {
                    // Generate Token array
                    let tokens: Vec<_> = sentence
                        .content_tokens
                        .iter()
                        .map(|token| match token {
                            TokenData::Text(text) => {
                                quote::quote! { &Token::Text(#text) }
                            }
                            TokenData::BoldText(text) => {
                                quote::quote! { &Token::BoldText(#text) }
                            }
                            TokenData::ItalicText(text) => {
                                quote::quote! { &Token::ItalicText(#text) }
                            }
                            TokenData::BoldItalicText(text) => {
                                quote::quote! { &Token::BoldItalicText(#text) }
                            }
                            TokenData::Code(text) => {
                                quote::quote! { &Token::Code(#text) }
                            }
                        })
                        .collect();

                    let character_expr = if let Some(ref char_name) = sentence.character {
                        quote::quote! { Some(#char_name) }
                    } else {
                        quote::quote! { None }
                    };

                    let next_step_expr = if let Some(ref next_step_id) = sentence.next_step {
                        let next_id_upper = next_step_id.to_uppercase().replace('-', "_");
                        let next_variant_name = syn::Ident::new(
                            &format!("R_{}", next_id_upper),
                            proc_macro2::Span::call_site(),
                        );
                        quote::quote! { Some(StepId::#next_variant_name) }
                    } else {
                        quote::quote! { None }
                    };

                    let silence_switch = sentence.silence_switch;
                    quote::quote! {
                        &Sentence {
                            character: #character_expr,
                            content_tokens: &[#(#tokens),*],
                            next_step: #next_step_expr,
                            silence_switch: #silence_switch,
                        }
                    }
                })
                .collect();

            let sentences_formatted = if sentences.is_empty() {
                quote::quote! { &[] }
            } else {
                quote::quote! { &[#(#sentences),*] }
            };

            quote::quote! {
                StepId::#variant_name => Some(Step {
                    sentences: #sentences_formatted,
                }),
            }
        })
        .collect();

    quote::quote! {
        pub use #mod_name::StepId;
        pub mod #mod_name {
            #[allow(non_camel_case_types)]
            pub enum StepId {
                #step_id_variants_formatted
            }

            pub struct Step<'a> {
                pub sentences: &'a [&'a Sentence<'a>],
            }

            pub struct Sentence<'a> {
                pub character: Option<&'a str>,
                pub content_tokens: &'a [&'a Token<'a>],
                pub next_step: Option<StepId>,
                pub silence_switch: bool,
            }

            pub enum Token<'a> {
                Text(&'a str),
                BoldText(&'a str),
                ItalicText(&'a str),
                BoldItalicText(&'a str),
                Code(&'a str),
            }

            impl<'a> std::fmt::Display for Token<'a> {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    match self {
                        Token::Text(text) => write!(f, "{}", text),
                        Token::BoldText(text) => write!(f, "**{}**", text),
                        Token::ItalicText(text) => write!(f, "*{}*", text),
                        Token::BoldItalicText(text) => write!(f, "***{}***", text),
                        Token::Code(text) => write!(f, "`{}`", text),
                    }
                }
            }

            pub fn get_step(id: StepId) -> Option<Step<'static>> {
                match id {
                    #(#match_arms)*
                }
            }
        }
    }
}

struct DialogFile {
    steps: Vec<StepData>,
}

struct StepData {
    id: String,
    sentences: Vec<SentenceData>,
}

struct SentenceData {
    character: Option<String>,
    content_tokens: Vec<TokenData>,
    next_step: Option<String>,
    silence_switch: bool,
}

enum TokenData {
    Text(String),
    BoldText(String),
    ItalicText(String),
    BoldItalicText(String),
    Code(String),
}
