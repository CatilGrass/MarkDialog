# MarkDialog

> 用 Markdown 来书写您的剧情！

## 一些碎碎语

这是一个我个人用于学习的项目，为了快速实现我想要的效果，项目中实验性地使用了 **Vibe Coding**

## 介绍

MarkDialog 是 **书写文字剧情的范式**，它定义了如何用 **Markdown** 来描述文字冒险游戏的对话。

它允许你：

- 使用 **六级标题** 表示角色
- 使用 **标题** 表示段落
- 使用 **超链接** 表示跳转
- 使用 **代码块** 来控制速度、画面，与游戏内容互动

> [!NOTE]
> 如果你想了解 Markdialog 的语法，可以点击此处
>
> [Markdialog 示例文章](#示例文章)

## 工具链

MarkDialog 提供了编译工具 `mdialogc`，它可以将您的 **Markdown** 文件编译成中间语言，以快速地在任何前端播放或执行。

```bash
# 执行，将会输出 YourMarkdown.dialog
mdialogc -i YourMarkdown.md
```

目前支持的前端：

- [Rust](#Rust 示例)

## 其他

#### 示例文章

```markdown
# 对话

###### 爱丽丝

早上好，鲍勃。

你吃早饭了么？

- [当然吃了！](#鲍勃吃过了早饭)
- [并没。](#鲍勃还没吃早饭)
- [我还不饿。](#鲍勃并不饿)

## 鲍勃吃过了早饭

###### 鲍勃

我当然**吃了早饭**！

###### 爱丽丝

*哦。*我还想你跟我一起去吃呢。

###### 旁白

(剧情结束)

## 鲍勃还没吃早饭

###### 鲍勃

并没吃。

###### 爱丽丝

[好啊！一起去吃早饭么？](#爱丽丝询问是否一起吃早饭)

## 鲍勃并不饿

###### 鲍勃

*我其实不饿*？

###### 爱丽丝

那算了，今天太遗憾了。（*小声嘟囔*）

###### 旁白

(剧情结束)

## 爱丽丝询问是否一起吃早饭

###### 鲍勃

让我想想。。。

- 走吧！
- 算了，我[并不饿](#鲍勃并不饿)

###### 爱丽丝

好啊！走吧！

###### 旁白

（于是鲍勃和爱丽丝一起吃了早饭）

（剧情结束）
```

#### Rust 示例

```toml
# Cargo.toml
[package]
name = "your_proj"
version = "0.1.0"
edition = "2024"

[dependencies]
markdialog = { git = "https://github.com/CatilGrass/MarkDialog" }
```

```rust
// main.rs
use markdialog::generate::{markdialog, step};

markdialog!(my = "alice_and_bob.dialog");

fn main() {
    let my_step = my::get_step(step!("鲍勃吃过了早饭")).unwrap();
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

