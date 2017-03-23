#[derive(Clone, Debug, PartialEq)]
pub enum Token<'a> {
    Id(&'a str),
    OpenParen,
    CloseParen,
    Asterisk,
    Colon,
    Comma,
    SemiColon,
    Arrow,
    Function,
    Const,
    Mut,
    Pub,
}

#[derive(Clone, PartialEq, Debug)]
pub struct TokenAndPos<'src> {
    pub tok: Token<'src>,
    pub pos: usize,
}

impl<'src> TokenAndPos<'src> {
    pub fn new(t: Token<'src>, p: usize) -> Self {
        TokenAndPos { tok: t, pos: p }
    }
}
