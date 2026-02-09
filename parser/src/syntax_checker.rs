use crate::error::Exit;

pub fn check_markdown_syntax(i: &String) -> Result<(), Exit> {
    let mut stack = Vec::new();
    let lines: Vec<&str> = i.lines().collect();
    let mut anchors = Vec::new();
    let mut heading_ids = Vec::new();

    for (line_num, line) in lines.iter().enumerate() {
        let line_num = line_num as i64 + 1;

        // Check for headings to collect anchor IDs
        if line.starts_with('#') {
            let heading_text = line.trim_start_matches('#').trim();
            let id = heading_text
                .to_lowercase()
                .chars()
                .filter(|c| c.is_alphanumeric() || *c == '-' || *c == '_')
                .collect::<String>();
            if !id.is_empty() {
                heading_ids.push(id);
            }
        }

        let mut chars = line.chars().enumerate().peekable();
        while let Some((pos, ch)) = chars.next() {
            let pos = pos as i64 + 1;

            match ch {
                '[' => {
                    // Check if it's a link or image
                    let is_image = chars.peek().map(|&(_, c)| c) == Some('!');
                    if is_image {
                        chars.next(); // Skip '!'
                    }
                    stack.push(('['.to_string(), line_num, pos, is_image));
                }
                ']' => {
                    if let Some((last, _l, b, is_image)) = stack.pop() {
                        if last != "[" {
                            return Err(Exit::SyntaxError {
                                content: line.to_string(),
                                reason: format!(
                                    "Mismatched bracket: expected '[' but found '{}'",
                                    last
                                ),
                                line: line_num,
                                begin: b,
                                end: pos,
                            });
                        }
                        // Check if it's followed by '(' for a link
                        if chars.peek().map(|&(_, c)| c) == Some('(') {
                            chars.next(); // Skip '('
                            // Look for closing ')'
                            let mut found = false;
                            let mut anchor_started = false;
                            let mut anchor = String::new();
                            while let Some((_, c)) = chars.next() {
                                if c == ')' {
                                    found = true;
                                    break;
                                }
                                if c == '#' && !anchor_started {
                                    anchor_started = true;
                                    continue;
                                }
                                if anchor_started {
                                    anchor.push(c);
                                }
                            }
                            if !found {
                                return Err(Exit::SyntaxError {
                                    content: line.to_string(),
                                    reason: "Link parentheses not closed".to_string(),
                                    line: line_num,
                                    begin: pos,
                                    end: pos,
                                });
                            }
                            if !anchor.is_empty() {
                                // Remove whitespace from anchor
                                let anchor = anchor.replace(|c: char| c.is_whitespace(), "");
                                anchors.push((anchor, line_num, pos));
                            }
                        } else if !is_image {
                            // It's a reference link, collect the anchor
                            // Check for anchor like [](#anchor)
                            if chars.peek().map(|&(_, c)| c) == Some('(') {
                                chars.next(); // Skip '('
                                if chars.peek().map(|&(_, c)| c) == Some('#') {
                                    chars.next(); // Skip '#'
                                    let mut anchor = String::new();
                                    while let Some(&(_, c)) = chars.peek() {
                                        if c == ')' {
                                            break;
                                        }
                                        anchor.push(c);
                                        chars.next();
                                    }
                                    if !anchor.is_empty() {
                                        // Remove whitespace from anchor
                                        let anchor =
                                            anchor.replace(|c: char| c.is_whitespace(), "");
                                        anchors.push((anchor, line_num, pos));
                                    }
                                }
                            }
                        }
                    } else {
                        return Err(Exit::SyntaxError {
                            content: line.to_string(),
                            reason: "Unmatched ']'".to_string(),
                            line: line_num,
                            begin: pos,
                            end: pos,
                        });
                    }
                }
                '(' => {
                    // Check for standalone anchor like (#anchor)
                    if chars.peek().map(|&(_, c)| c) == Some('#') {
                        chars.next(); // Skip '#'
                        let mut anchor = String::new();
                        while let Some(&(_, c)) = chars.peek() {
                            if c == ')' {
                                break;
                            }
                            anchor.push(c);
                            chars.next();
                        }
                        if !anchor.is_empty() {
                            // Remove whitespace from anchor
                            let anchor = anchor.replace(|c: char| c.is_whitespace(), "");
                            anchors.push((anchor, line_num, pos));
                        }
                    } else {
                        stack.push(('('.to_string(), line_num, pos, false));
                    }
                }
                ')' => {
                    if let Some((last, _l, b, _)) = stack.pop() {
                        if last != "(" {
                            return Err(Exit::SyntaxError {
                                content: line.to_string(),
                                reason: format!(
                                    "Mismatched parenthesis: expected '(' but found '{}'",
                                    last
                                ),
                                line: line_num,
                                begin: b,
                                end: pos,
                            });
                        }
                    } else {
                        return Err(Exit::SyntaxError {
                            content: line.to_string(),
                            reason: "Unmatched ')'".to_string(),
                            line: line_num,
                            begin: pos,
                            end: pos,
                        });
                    }
                }
                '`' => {
                    // Check for backticks
                    let mut count = 1;
                    while chars.peek().map(|&(_, c)| c) == Some('`') {
                        count += 1;
                        chars.next();
                    }
                    let marker = "`".repeat(count);

                    if let Some((last, _, _, _)) = stack.last() {
                        if last == &marker {
                            stack.pop();
                        } else {
                            stack.push((marker.clone(), line_num, pos, false));
                        }
                    } else {
                        stack.push((marker, line_num, pos, false));
                    }
                }
                _ => {}
            }
        }
    }

    // Check for unclosed brackets/parentheses
    if let Some((last, line, begin, _)) = stack.pop() {
        return Err(Exit::SyntaxError {
            content: lines[(line - 1) as usize].to_string(),
            reason: format!("Unclosed '{}'", last),
            line,
            begin,
            end: begin,
        });
    }

    // Check if all anchors have corresponding headings
    for (anchor, line_num, pos) in anchors {
        // Normalize anchor for comparison: convert to lowercase and filter characters
        let normalized_anchor = anchor
            .to_lowercase()
            .chars()
            .filter(|c| c.is_alphanumeric() || *c == '-' || *c == '_')
            .collect::<String>();

        if !heading_ids.contains(&normalized_anchor) {
            return Err(Exit::SyntaxError {
                content: lines[(line_num - 1) as usize].to_string(),
                reason: format!("Anchor '#{}' has no corresponding heading", anchor),
                line: line_num,
                begin: pos,
                end: pos + anchor.len() as i64,
            });
        }
    }

    Ok(())
}
