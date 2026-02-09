use std::path::PathBuf;

use regex::Regex;
use sha2::{Digest, Sha256};

use crate::{error::Exit, syntax_checker::check_markdown_syntax};

pub fn parse(input: PathBuf, ir_output: PathBuf) -> Result<(), Exit> {
    let result = std::fs::read_to_string(&input)?;

    check_markdown_syntax(&result)?;

    let result = clean_markdown(result)?;
    let result = fix_mark_jump(result)?;
    let result = replace_marker_name(result)?;
    let result = convert_to_step_sentence_structure(result)?;
    let result = strip_invalid_jump(result)?;

    std::fs::write(&ir_output, result)?;
    Ok(())
}

/// Clean Markdown
/// 1. Remove blockquotes
/// 2. Remove empty lines
/// 3. Trim each line
pub fn clean_markdown(i: String) -> Result<String, Exit> {
    let lines = i.lines();
    let mut cleaned = Vec::new();

    for line in lines {
        if line.starts_with('>') {
            continue;
        }
        let trimmed = line.trim();
        if trimmed.is_empty() {
            continue;
        }
        cleaned.push(trimmed.to_string());
    }

    Ok(cleaned.join("\n"))
}

#[cfg(test)]
mod test_clean_markdown {
    use super::*;

    #[test]
    fn test_clean_markdown_removes_blockquotes() {
        let input = "> This is a blockquote\nNormal text\n> Another blockquote".to_string();
        let expected = "Normal text".to_string();
        let Ok(result) = clean_markdown(input) else {
            panic!("Parse error!");
        };
        assert_eq!(result, expected);
    }

    #[test]
    fn test_clean_markdown_removes_empty_lines() {
        let input = "Line 1\n\n\nLine 2\n\n".to_string();
        let expected = "Line 1\nLine 2".to_string();
        let Ok(result) = clean_markdown(input) else {
            panic!("Parse error!");
        };
        assert_eq!(result, expected);
    }

    #[test]
    fn test_clean_markdown_trims_lines() {
        let input = "  Line 1  \n\tLine 2\t\n".to_string();
        let expected = "Line 1\nLine 2".to_string();
        let Ok(result) = clean_markdown(input) else {
            panic!("Parse error!");
        };
        assert_eq!(result, expected);
    }

    #[test]
    fn test_clean_markdown_combined() {
        let input = "> Blockquote\n\n  Line 1  \n> Another\n\nLine 2\n\n".to_string();
        let expected = "Line 1\nLine 2".to_string();
        let Ok(result) = clean_markdown(input) else {
            panic!("Parse error!");
        };
        assert_eq!(result, expected);
    }

    #[test]
    fn test_clean_markdown_empty_input() {
        let input = "".to_string();
        let expected = "".to_string();
        let Ok(result) = clean_markdown(input) else {
            panic!("Parse error!");
        };
        assert_eq!(result, expected);
    }

    #[test]
    fn test_clean_markdown_only_blockquotes() {
        let input = "> Quote 1\n> Quote 2".to_string();
        let expected = "".to_string();
        let Ok(result) = clean_markdown(input) else {
            panic!("Parse error!");
        };
        assert_eq!(result, expected);
    }

    #[test]
    fn test_clean_markdown_only_whitespace() {
        let input = "   \n\t\n  ".to_string();
        let expected = "".to_string();
        let Ok(result) = clean_markdown(input) else {
            panic!("Parse error!");
        };
        assert_eq!(result, expected);
    }
}

/// Fix jump syntax in each line
/// 1. Correct the following syntax
/// ```ignore
/// - It's [Item](#Mark)
/// > corrected to
/// - It's Item [](#Mark)
/// ```
///
/// 2. If there are multiple options, take the first one
/// ```ignore
/// - There might be two options: [A](#A) and [B](#B)!
/// > corrected to
/// - There might be two options: A and B! [](#A)
/// ```
pub fn fix_mark_jump(i: String) -> Result<String, Exit> {
    let mut result = String::new();

    for line in i.lines() {
        let (processed_content, first_link_dest) = helper_process_line_content(line);
        let processed_line = helper_format_line_with_link(processed_content, first_link_dest);
        let final_line = helper_convert_ordered_list_marker(processed_line);

        result.push_str(&final_line);
        result.push('\n');
    }

    if result.ends_with('\n') {
        result.pop();
    }

    Ok(result)
}

/// Process line content, extract link text and return the first link target
///
/// # Examples
///
/// ```
/// use markdialog_parser::parse::helper_process_line_content;
///
/// // Single link
/// let (content, dest) = helper_process_line_content("This is a [Link](#target) Example");
/// assert_eq!(content, "This is a Link Example");
/// assert_eq!(dest, Some("target".to_string()));
///
/// // Extract the first link
/// let (content, dest) = helper_process_line_content("First [link1](#target1) and second [link2](#target2)");
/// assert_eq!(content, "First link1 and second link2");
/// assert_eq!(dest, Some("target1".to_string()));
///
/// // No link
/// let (content, dest) = helper_process_line_content("Text without link");
/// assert_eq!(content, "Text without link");
/// assert_eq!(dest, None);
///
/// // Invalid link
/// let (content, dest) = helper_process_line_content("Invalid [link format");
/// assert_eq!(content, "Invalid [");
/// assert_eq!(dest, None);
///
/// // Empty
/// let (content, dest) = helper_process_line_content("");
/// assert_eq!(content, "");
/// assert_eq!(dest, None);
///
/// // Link target contains spaces and extra # symbols
/// let (content, dest) = helper_process_line_content("Link[text](#  target#)");
/// assert_eq!(content, "Linktext");
/// assert_eq!(dest, Some("target".to_string()));
/// ```
pub fn helper_process_line_content(line: &str) -> (String, Option<String>) {
    // Check if line is an image line (starts with "![")
    if line.starts_with("![") {
        // Return the original line unchanged with no link destination
        return (line.to_string(), None);
    }

    let mut processed = String::new();
    let mut chars = line.chars().peekable();
    let mut first_link_dest = None;
    let mut has_link = false;

    while let Some(ch) = chars.next() {
        if ch == '[' {
            if let Some((link_text, link_dest, remaining_chars)) = helper_parse_link(&mut chars) {
                processed.push_str(&link_text);
                if !has_link {
                    first_link_dest = Some(link_dest);
                    has_link = true;
                }
                chars = remaining_chars;
                continue;
            } else {
                // Invalid
                processed.push(ch);
            }
        } else {
            processed.push(ch);
        }
    }

    (processed, first_link_dest)
}

/// Parse possible Markdown links, return (link text, link target, remaining character iterator)
///
/// # Examples
///
/// ```
/// use markdialog_parser::parse::helper_parse_link;
///
/// // Standard Link
/// let mut chars = "[Link](#target)".chars().peekable();
/// chars.next(); // Skip '['
/// let result = helper_parse_link(&mut chars);
/// assert!(result.is_some());
/// let (text, dest, _) = result.unwrap();
/// assert_eq!(text, "Link");
/// assert_eq!(dest, "target");
///
/// // Link text contains spaces
/// let mut chars = "[Link text](#target)".chars().peekable();
/// chars.next();
/// let result = helper_parse_link(&mut chars);
/// assert!(result.is_some());
/// let (text, dest, _) = result.unwrap();
/// assert_eq!(text, "Link text");
/// assert_eq!(dest, "target");
///
/// // Link target contains spaces and extra # symbols
/// let mut chars = "[text](#  target#)".chars().peekable();
/// chars.next();
/// let result = helper_parse_link(&mut chars);
/// assert!(result.is_some());
/// let (text, dest, _) = result.unwrap();
/// assert_eq!(text, "text");
/// assert_eq!(dest, "target");
///
/// // Invalid format: missing ']'
/// let mut chars = "[Link(#target)".chars().peekable();
/// chars.next();
/// let result = helper_parse_link(&mut chars);
/// assert!(result.is_none());
///
/// // Invalid format: missing '(#'
/// let mut chars = "[Link]target)".chars().peekable();
/// chars.next();
/// let result = helper_parse_link(&mut chars);
/// assert!(result.is_none());
///
/// // Invalid format: missing ')'
/// let mut chars = "[Link](#target".chars().peekable();
/// chars.next();
/// let result = helper_parse_link(&mut chars);
/// assert!(result.is_some());
/// let (text, dest, _) = result.unwrap();
/// assert_eq!(text, "Link");
/// assert_eq!(dest, "target");
/// ```
pub fn helper_parse_link<'a>(
    chars: &mut std::iter::Peekable<std::str::Chars<'a>>,
) -> Option<(String, String, std::iter::Peekable<std::str::Chars<'a>>)> {
    let mut link_text = String::new();

    while let Some(&ch) = chars.peek() {
        chars.next();
        if ch == ']' {
            break;
        }
        link_text.push(ch);
    }

    if chars.next() != Some('(') || chars.next() != Some('#') {
        return None;
    }

    let mut link_dest = String::new();
    while let Some(ch) = chars.next() {
        if ch == ')' {
            break;
        }
        link_dest.push(ch);
    }

    let cleaned_dest = link_dest.trim().replace(' ', "").replace('#', "");

    Some((link_text, cleaned_dest, chars.clone()))
}

/// If there is a link dest, add a jump marker at the end of the line
///
/// # Examples
///
/// ```
/// use markdialog_parser::parse::helper_format_line_with_link;
///
/// // With a link dest
/// let content = "Some content".to_string();
/// let link_dest = Some("target".to_string());
/// let result = helper_format_line_with_link(content, link_dest);
/// assert_eq!(result, "Some content [](#target)");
///
/// // With empty content and a link dest
/// let content = "".to_string();
/// let link_dest = Some("target".to_string());
/// let result = helper_format_line_with_link(content, link_dest);
/// assert_eq!(result, "[](#target)");
///
/// // With trailing spaces in content
/// let content = "Content with spaces   ".to_string();
/// let link_dest = Some("target".to_string());
/// let result = helper_format_line_with_link(content, link_dest);
/// assert_eq!(result, "Content with spaces [](#target)");
///
/// // Without a link dest
/// let content = "Some content".to_string();
/// let link_dest = None;
/// let result = helper_format_line_with_link(content, link_dest);
/// assert_eq!(result, "Some content");
///
/// // With an empty link dest
/// let content = "Some content".to_string();
/// let link_dest = Some("".to_string());
/// let result = helper_format_line_with_link(content, link_dest);
/// assert_eq!(result, "Some content");
///
/// // With whitespace-only link dest
/// let content = "Some content".to_string();
/// let link_dest = Some("   ".to_string());
/// let result = helper_format_line_with_link(content, link_dest);
/// assert_eq!(result, "Some content");
/// ```
pub fn helper_format_line_with_link(content: String, link_dest: Option<String>) -> String {
    match link_dest {
        Some(dest) if !dest.trim().is_empty() => {
            format!("{} [](#{})", content.trim_end(), dest.trim())
                .trim()
                .to_string()
        }
        _ => content,
    }
}

/// Convert ordered list markers to unordered list markers
///
/// # Examples
///
/// ```
/// use markdialog_parser::parse::helper_convert_ordered_list_marker;
///
/// // Basic conversion
/// let input = "1. First item".to_string();
/// let result = helper_convert_ordered_list_marker(input);
/// assert_eq!(result, "- First item");
///
/// // Multi-digit numbers
/// let input = "10. Tenth item".to_string();
/// let result = helper_convert_ordered_list_marker(input);
/// assert_eq!(result, "- Tenth item");
///
/// // With leading spaces
/// let input = "  2. Second item".to_string();
/// let result = helper_convert_ordered_list_marker(input);
/// assert_eq!(result, "- Second item");
///
/// // Not an ordered list marker (no dot and space)
/// let input = "1.Not a list".to_string();
/// let result = helper_convert_ordered_list_marker(input);
/// assert_eq!(result, "1.Not a list");
///
/// // Not an ordered list marker (different spacing)
/// let input = "1.  Extra space".to_string();
/// let result = helper_convert_ordered_list_marker(input);
/// assert_eq!(result, "-  Extra space");
///
/// // Already unordered list
/// let input = "- Already unordered".to_string();
/// let result = helper_convert_ordered_list_marker(input);
/// assert_eq!(result, "- Already unordered");
///
/// // Regular text
/// let input = "This is not a list".to_string();
/// let result = helper_convert_ordered_list_marker(input);
/// assert_eq!(result, "This is not a list");
///
/// // Empty string
/// let input = "".to_string();
/// let result = helper_convert_ordered_list_marker(input);
/// assert_eq!(result, "");
///
/// // Only whitespace
/// let input = "   ".to_string();
/// let result = helper_convert_ordered_list_marker(input);
/// assert_eq!(result, "   ");
/// ```
pub fn helper_convert_ordered_list_marker(line: String) -> String {
    let trimmed = line.trim_start();

    if let Some(_rest) = trimmed.strip_prefix(|c: char| c.is_ascii_digit()) {
        let mut chars = trimmed.chars();
        let mut digit_count = 0;

        while let Some(c) = chars.next() {
            if c.is_ascii_digit() {
                digit_count += 1;
            } else {
                break;
            }
        }

        if digit_count > 0 {
            let rest_after_digits = &trimmed[digit_count..];
            if let Some(content) = rest_after_digits.strip_prefix(". ") {
                return format!("- {}", content);
            }
        }
    }

    line
}

#[cfg(test)]
mod test_fix_mark_jump {
    use super::*;

    #[test]
    fn test_fix_mark_jump_single_link() {
        let input = "- It's [Item](#Mark)".to_string();
        let expected = "- It's Item [](#Mark)".to_string();
        let Ok(result) = fix_mark_jump(input) else {
            panic!("Parse error!");
        };
        assert_eq!(result, expected);
    }

    #[test]
    fn test_fix_mark_jump_multiple_links_takes_first() {
        let input = "- There might be two options: [A](#A) and [B](#B)!".to_string();
        let expected = "- There might be two options: A and B! [](#A)".to_string();
        let Ok(result) = fix_mark_jump(input) else {
            panic!("Parse error!");
        };
        assert_eq!(result, expected);
    }

    #[test]
    fn test_fix_mark_jump_no_link() {
        let input = "- Just a normal line".to_string();
        let expected = "- Just a normal line".to_string();
        let Ok(result) = fix_mark_jump(input) else {
            panic!("Parse error!");
        };
        assert_eq!(result, expected);
    }

    #[test]
    fn test_fix_mark_jump_empty_line() {
        let input = "".to_string();
        let expected = "".to_string();
        let Ok(result) = fix_mark_jump(input) else {
            panic!("Parse error!");
        };
        assert_eq!(result, expected);
    }

    #[test]
    fn test_fix_mark_jump_multiple_lines() {
        let input = "- First [Item](#First)\n- Second [Item](#Second)".to_string();
        let expected = "- First Item [](#First)\n- Second Item [](#Second)".to_string();
        let Ok(result) = fix_mark_jump(input) else {
            panic!("Parse error!");
        };
        assert_eq!(result, expected);
    }

    #[test]
    fn test_fix_mark_jump_link_at_end() {
        let input = "- End with [link](#target)".to_string();
        let expected = "- End with link [](#target)".to_string();
        let Ok(result) = fix_mark_jump(input) else {
            panic!("Parse error!");
        };
        assert_eq!(result, expected);
    }

    #[test]
    fn test_fix_mark_jump_link_at_beginning() {
        let input = "- [Start](#target) with link".to_string();
        let expected = "- Start with link [](#target)".to_string();
        let Ok(result) = fix_mark_jump(input) else {
            panic!("Parse error!");
        };
        assert_eq!(result, expected);
    }

    #[test]
    fn test_fix_mark_jump_link_in_middle() {
        let input = "- Text [middle](#target) text".to_string();
        let expected = "- Text middle text [](#target)".to_string();
        let Ok(result) = fix_mark_jump(input) else {
            panic!("Parse error!");
        };
        assert_eq!(result, expected);
    }

    #[test]
    fn test_fix_mark_jump_ordered_list_conversion() {
        let input = "1. [Item](#target)".to_string();
        let expected = "- Item [](#target)".to_string();
        let Ok(result) = fix_mark_jump(input) else {
            panic!("Parse error!");
        };
        assert_eq!(result, expected);
    }

    #[test]
    fn test_fix_mark_jump_ordered_list_multiple_digits() {
        let input = "10. [Tenth](#target) item".to_string();
        let expected = "- Tenth item [](#target)".to_string();
        let Ok(result) = fix_mark_jump(input) else {
            panic!("Parse error!");
        };
        assert_eq!(result, expected);
    }

    #[test]
    fn test_fix_mark_jump_mixed_ordered_and_unordered() {
        let input = "1. [First](#first)\n- [Second](#second)\n2. [Third](#third)".to_string();
        let expected = "- First [](#first)\n- Second [](#second)\n- Third [](#third)".to_string();
        let Ok(result) = fix_mark_jump(input) else {
            panic!("Parse error!");
        };
        assert_eq!(result, expected);
    }

    #[test]
    fn test_fix_mark_jump_invalid_link_format() {
        let input = "- Invalid [link format".to_string();
        let expected = "- Invalid [".to_string();
        let Ok(result) = fix_mark_jump(input) else {
            panic!("Parse error!");
        };
        assert_eq!(result, expected);
    }

    #[test]
    fn test_fix_mark_jump_link_with_spaces_in_target() {
        let input = "- Link [text](#  target#)".to_string();
        let expected = "- Link text [](#target)".to_string();
        let Ok(result) = fix_mark_jump(input) else {
            panic!("Parse error!");
        };
        assert_eq!(result, expected);
    }

    #[test]
    fn test_fix_mark_jump_empty_link_text() {
        let input = "- [](#target)".to_string();
        let expected = "- [](#target)".to_string();
        let Ok(result) = fix_mark_jump(input) else {
            panic!("Parse error!");
        };
        assert_eq!(result, expected);
    }

    #[test]
    fn test_fix_mark_jump_only_whitespace() {
        let input = "   ".to_string();
        let expected = "   ".to_string();
        let Ok(result) = fix_mark_jump(input) else {
            panic!("Parse error!");
        };
        assert_eq!(result, expected);
    }

    #[test]
    fn test_fix_mark_jump_complex_multiple_links() {
        let input = "- Choose [A](#A), [B](#B), or [C](#C)!".to_string();
        let expected = "- Choose A, B, or C! [](#A)".to_string();
        let Ok(result) = fix_mark_jump(input) else {
            panic!("Parse error!");
        };
        assert_eq!(result, expected);
    }
}

/// Replace marker names: replace heading text and link anchors with corresponding SHA256
///
/// Example:
/// ```ignore
/// # Original text
/// # Chapter Title
/// - Jump to [Chapter Title](#Chapter Title)
///
/// # After processing
/// # a1b2c3d4
/// - Jump to [](#a1b2c3d4)
/// ```
pub fn replace_marker_name(i: String) -> Result<String, Exit> {
    let mut result = i;

    let heading_re = Regex::new(r"^(#{1,5})\s+(.+)$").unwrap();
    let mut heading_map = std::collections::HashMap::new();

    for line in result.lines() {
        if let Some(caps) = heading_re.captures(line) {
            let heading_text = caps[2].trim().to_string();
            let hash = format!("{:x}", Sha256::digest(heading_text.as_bytes()));
            let short_hash = &hash[..8];
            heading_map.insert(heading_text, short_hash.to_string());
        }
    }

    let mut lines: Vec<String> = Vec::new();
    for line in result.lines() {
        if let Some(caps) = heading_re.captures(line) {
            let level = &caps[1];
            let heading_text = caps[2].trim();

            if let Some(hash) = heading_map.get(heading_text) {
                lines.push(format!("{} {}", level, hash));
            } else {
                lines.push(line.to_string());
            }
        } else {
            lines.push(line.to_string());
        }
    }
    result = lines.join("\n");

    let link_re = Regex::new(r"\[\]\(#([^)]+)\)").unwrap();
    result = link_re
        .replace_all(&result, |caps: &regex::Captures| {
            let anchor_name = &caps[1];
            if let Some(hash) = heading_map.get(anchor_name) {
                format!("[](#{})", hash)
            } else {
                let hash = format!("{:x}", Sha256::digest(anchor_name.as_bytes()));
                let short_hash = &hash[..8];
                format!("[](#{})", short_hash)
            }
        })
        .to_string();

    Ok(result)
}

/// Split content into Step + Sentence structure
pub fn convert_to_step_sentence_structure(input: String) -> Result<String, Exit> {
    let mut result = String::new();
    let mut current_marker = String::new();
    let mut current_step_id = 0;
    let mut current_character = String::new();
    let mut has_no_switch_flag = false;

    let mut code_record_mode = false;
    let mut option_record_mode = false;

    let mut sentences_buffer = String::new();
    for line in input.split("\n") {
        // Record code
        if code_record_mode {
            // If code block marker is found again, end code recording
            if line.starts_with("```") && code_record_mode {
                sentences_buffer.push_str("`\n");
                code_record_mode = false;
                continue;
            }
            sentences_buffer.push_str(format!("{}\\n", line).as_str());
            continue;
        }

        // Record options
        if option_record_mode {
            // Still an option, continue appending
            if line.starts_with("- ") {
                let (sentence, next) = helper_get_jump_from_line(line);
                let next = if let Some(next) = next {
                    format!("->[#{}_0]", next)
                } else {
                    next_flag(current_marker.as_str(), current_step_id)
                };
                let option_line = format!(
                    "{}[{}]{}",
                    character(&current_character, has_no_switch_flag),
                    sentence,
                    next
                );
                sentences_buffer.push_str(option_line.as_str());
                sentences_buffer.push('\n');
                continue;
            } else {
                // When ending option recording, create and advance one Step
                result.push_str(step_line(current_marker.as_str(), current_step_id).as_str());
                result.push('\n');
                result.push_str(sentences_buffer.as_str());
                sentences_buffer.clear();
                current_step_id += 1;
                // Clean "Has no switch flag"
                has_no_switch_flag = false;
                // Close option mode
                option_record_mode = false;
                // Do not continue here, proceed to process subsequent content
            }
        }

        // Refresh heading
        if helper_is_marker(line) {
            current_marker = helper_read_maker(line).to_string();
            current_step_id = 0;
            continue;
        }

        // Refresh character
        if helper_is_character(line) {
            let (character, no_switch_flag) = helper_read_character(line);
            current_character = character.to_string();
            has_no_switch_flag = no_switch_flag;
            continue;
        }

        // Image recording
        if line.starts_with('!') {
            sentences_buffer.push_str(line);
            sentences_buffer.push('\n');
            continue;
        }

        // Start code recording
        if line.starts_with("```") && !code_record_mode {
            sentences_buffer.push('`');
            code_record_mode = true;
            continue;
        }

        // Option recording
        if line.starts_with("- ") {
            let (sentence, next) = helper_get_jump_from_line(line);
            let next = if let Some(next) = next {
                format!("->[#{}_0]", next)
            } else {
                next_flag(current_marker.as_str(), current_step_id)
            };
            let option_line = format!(
                "{}[{}]{}",
                character(&current_character, has_no_switch_flag),
                sentence,
                next
            );
            sentences_buffer.push_str(option_line.as_str());
            sentences_buffer.push('\n');

            // Start option recording mode
            if !option_record_mode {
                option_record_mode = true;
            }
            continue;
        }

        // Normal sentence
        let (sentence, next) = helper_get_jump_from_line(line);
        let next = if let Some(next) = next {
            format!("->[#{}_0]", next)
        } else {
            next_flag(current_marker.as_str(), current_step_id)
        };
        let sentence_line = format!(
            "{}[{}]{}",
            character(&current_character, has_no_switch_flag),
            sentence,
            next
        );
        has_no_switch_flag = false;

        // Create and advance one Step
        result.push_str(step_line(current_marker.as_str(), current_step_id).as_str());
        result.push('\n');
        result.push_str(sentences_buffer.as_str());
        sentences_buffer.clear();
        result.push_str(sentence_line.as_str());
        result.push('\n');
        current_step_id += 1;
    }

    Ok(result)
}

pub fn character(character: &str, has_no_switch_flag: bool) -> String {
    let flag = if has_no_switch_flag { "*" } else { "" };
    format!("[{}{}{}]:", &flag, character, &flag)
}

pub fn step_name(marker: &str, current_id: i64) -> String {
    format!("{}_{}", marker, current_id)
}

pub fn step_line(marker: &str, current_id: i64) -> String {
    format!("@@@@@@@@@@ {}_{}", marker, current_id)
}

pub fn next_flag(marker: &str, current_id: i64) -> String {
    format!("->[#{}_{}]", marker, current_id + 1)
}

pub fn helper_is_marker(line: &str) -> bool {
    line.starts_with("# ")
        || line.starts_with("## ")
        || line.starts_with("### ")
        || line.starts_with("#### ")
        || line.starts_with("##### ")
}

pub fn helper_read_maker(line: &str) -> &str {
    let trimmed = line.trim_start();
    if trimmed.starts_with('#') {
        if trimmed.starts_with("# ")
            || trimmed.starts_with("## ")
            || trimmed.starts_with("### ")
            || trimmed.starts_with("#### ")
            || trimmed.starts_with("##### ")
        {
            let parts: Vec<&str> = trimmed.splitn(2, ' ').collect();
            if parts.len() == 2 {
                return parts[1].trim();
            }
        }
    }
    ""
}

pub fn helper_is_character(line: &str) -> bool {
    line.starts_with("######")
}

pub fn helper_read_character(line: &str) -> (&str, bool) {
    let trimmed = line.trim_start();
    if trimmed.starts_with("######") {
        let parts: Vec<&str> = trimmed.splitn(2, ' ').collect();
        if parts.len() == 2 {
            let character = parts[1].trim();
            if character.starts_with('*') && character.ends_with('*') {
                let trimmed = character.trim_matches('*');
                return (trimmed.trim(), true);
            } else {
                return (character.trim(), false);
            }
        }
    }
    ("", false)
}

pub fn helper_get_jump_from_line(line: &str) -> (String, Option<String>) {
    let pattern = r"\[\]\(#([^)]+)\)$";
    let re = Regex::new(pattern).unwrap();

    if let Some(caps) = re.captures(line.trim_end()) {
        let target = caps.get(1).unwrap().as_str();
        let line_without_jump = line
            .trim_end()
            .replace(&format!(" [](#{})", target), "")
            .to_string();
        return (
            line_without_jump.trim_start_matches("- ").to_string(),
            Some(format!("{}", target)),
        );
    }

    (line.trim_start_matches("- ").to_string(), None)
}

/// Strip all jumps that have not appeared
pub fn strip_invalid_jump(input: String) -> Result<String, Exit> {
    let lines: Vec<&str> = input.lines().collect();
    let mut valid_ids = std::collections::HashSet::new();

    for line in &lines {
        if line.starts_with("@@@@@@@@@@ ") {
            let id = line.trim_start_matches("@@@@@@@@@@ ").trim();
            valid_ids.insert(id.to_string());
        }
    }

    let mut result_lines = Vec::new();
    let link_re = Regex::new(r"\[#([^)]+)\]").unwrap();

    for line in lines {
        let processed_line = link_re.replace_all(line, |caps: &regex::Captures| {
            let id = &caps[1];
            if valid_ids.contains(id) {
                format!("[#{}]", id)
            } else {
                "[]".to_string()
            }
        });
        result_lines.push(processed_line.to_string());
    }

    Ok(result_lines.join("\n"))
}
