use crate::structs::sentence::{Sentence, Token};

#[derive(Hash, PartialEq, Eq)]
pub enum SentenceId {
    // 在此处确认所有跳转点
    Main0,
    Main1,
    Main2,
    Ok1,
}

pub fn get_sentence(id: SentenceId) -> Option<Sentence<'static>> {
    match id {
        SentenceId::Main0 => Some(Sentence {
            content_tokens: &[
                &Token::Text("你好我是"),
                &Token::Command("red"),
                &Token::Text("猫尾草"),
                &Token::Command("/"),
            ],
            next_sentence: Some(SentenceId::Main1),
        }),
        SentenceId::Main1 => Some(Sentence {
            content_tokens: &[
                &Token::Text("你好我是"),
                &Token::Command("red"),
                &Token::Text("猫尾草"),
                &Token::Command("/"),
            ],
            next_sentence: Some(SentenceId::Main2),
        }),
        SentenceId::Main2 => Some(Sentence {
            content_tokens: &[
                &Token::Text("你好我是"),
                &Token::Command("red"),
                &Token::Text("猫尾草"),
                &Token::Command("/"),
            ],
            next_sentence: Some(SentenceId::Ok1),
        }),
        SentenceId::Ok1 => Some(Sentence {
            content_tokens: &[
                &Token::Text("你好我是"),
                &Token::Command("red"),
                &Token::Text("猫尾草"),
                &Token::Command("/"),
            ],
            next_sentence: None,
        }),
    }
}
