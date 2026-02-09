pub struct Sentence<'a> {
    pub content_tokens: &'a [&'static Token],
    pub next_sentence: Option<crate::res_sentences::SentenceId>,
}

pub enum Token {
    Text(&'static str),
    BoldText(&'static str),
    ItalicText(&'static str),
    BoldItalicText(&'static str),
    Command(&'static str),
}
