# MarkDialog

> Write your story with Markdown!

## Some Ramblings

This is a personal learning project of mine. To quickly achieve the desired effect, the project experimentally uses **Vibe Coding**.

## Introduction

MarkDialog is a **paradigm for writing textual narratives**. It defines how to use **Markdown** to describe dialogue for text adventure games.

It allows you to:

- Use **level-six headings** to represent characters.
- Use **headings** to represent paragraphs.
- Use **hyperlinks** to represent jumps.
- Use **code blocks** to control speed, visuals, and interact with game content.

> [!NOTE]
> If you want to learn about MarkDialog syntax, click here.
>
> [MarkDialog Example Article](#example-article)

## Toolchain

MarkDialog provides a compilation tool `mdialogc`, which can compile your **Markdown** files into an intermediate language for fast playback or execution on any frontend.

```bash
# Execution, will output YourMarkdown.dialog
mdialogc -i YourMarkdown.md
```

Currently supported frontends:

- [Rust](#rust-example)

## Other

#### Example Article

```markdown
# Dialogue

###### Alice

Good morning, Bob.

Have you had breakfast?

- [Of course I have!](#Bob-has-had-breakfast)
- [Not yet.](#Bob-has-not-had-breakfast)
- [I'm not hungry.](#Bob-is-not-hungry)

## Bob-has-had-breakfast

###### Bob

I certainly **had breakfast**!

###### Alice

*Oh.* I was hoping you'd join me.

###### Narrator

(Story ends)

## Bob-has-not-had-breakfast

###### Bob

Not yet.

###### Alice

[Great! Want to go get breakfast together?](#Alice-asks-to-eat-breakfast-together)

## Bob-is-not-hungry

###### Bob

*I'm actually not hungry*?

###### Alice

Never mind then, what a pity today. (*muttering quietly*)

###### Narrator

(Story ends)

## Alice-asks-to-eat-breakfast-together

###### Bob

Let me think...

- Let's go!
- No, I'm [not hungry](#Bob-is-not-hungry)

###### Alice

Great! Let's go!

###### Narrator

(And so Bob and Alice had breakfast together)

(Story ends)
```

#### Rust Example

```toml
# Cargo.toml
[package]
name = "your_proj"
version = "0.1.0"
edition = "2024"

[dependencies]
markdialog = "0.1"
```

```rust
// main.rs
use markdialog::{markdialog, step};

markdialog!(my = "alice_and_bob.dialog");

fn main() {
    let my_step = my::get_step(step!("Bob-has-had-breakfast")).unwrap();
    let sentence = my_step.sentences[0];
    println!(
        "{} said : \"{}\"",
        sentence.character.unwrap_or_default(),
        sentence
            .content_tokens
            .iter()
            .map(|token| match token {
                my::Token::Text(t) => t,
                my::Token::BoldText(t) => t,
                my::Token::ItalicText(t) => t,
                my::Token::BoldItalicText(t) => t,
                _ => "",
            }
            .to_string())
            .collect::<Vec<String>>()
            .join("")
    )
}
```
