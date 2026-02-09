# MarkDialog

> Write your story with Markdown!

> [!WARNING]
> This article is translated from `README_zh_CN.md`

> [!NOTE]
> This is a project I tinkered with in my spare time, and many aspects are still rough around the edges.
>
> I also experimentally used **Vibe Coding** in it, so you might encounter some unconventional code. Please bear with me.

## Why MarkDialog?

The starting point for this project was actually quite simple: I wanted to prototype an AVG visual novel and needed people around me to **be able to start writing the story immediately**.

But the reality is:

- Graphical editors are too heavy.
- Ink and Yarn Spinner are powerful, but both require learning.
- Writing in JSON / XML / custom DSLs isn't "natural" enough.

What I really wanted was a format that people already know how to write, requires no learning, can be opened and written in immediately, and can be run right after writing.

Then it suddenly hit me:

### Isn't Markdown exactly that?

You can easily write it in **Typora**, **Obsidian**, **VS Code**, or even **Vim/NeoVim**.

> What more could you ask for?

When this idea popped into my head, I got really excited.

And so, MarkDialog was born.



## What is MarkDialog?

It allows you to:

- Use level-six headings to switch characters.
- Use regular Markdown for dialogue lines.
- Use lists for choices.
- Use links for jumps.
- Use code blocks for rich text controls like speed, color, and actions.
- Use images to change backgrounds.

The Markdown you write is compiled into an IR (Intermediate Representation), which can then be parsed anywhere you want!

For example:

- Inline it into a Rust project.
- Import it into Unity / Unreal / Godot.
- Convert it to JSON.
- Convert it to anything.



## MarkDialog Syntax?

Generally speaking, if you know how to write Markdown, you know how to write MarkDialog.

You can write:

```
# Title
> Use blockquotes for comments

###### Zhang San
Use a level-six heading for **character names**.

Use unordered lists for choices
- Good morning
- Good afternoon
- Good evening

Ordered lists work too
1. Good morning
2. Good afternoon
3. Good evening

> Use hyperlinks for jumps
What's for lunch?
- Pizza [](#Eat_Pizza)
- Pasta [](#Eat_Pasta)
- Nothing
Or *eat nothing at all*!

## Eat_Pizza
Eat pizza

## Eat_Pasta
Eat pasta

> Use images to switch backgrounds
![](backgrounds/park)

> Use the following method to inline other files!
[[branch.md]]
```

It's that simple!



## Open Source License

Haha, I'm using the MIT License. Feel free to play around with it!
