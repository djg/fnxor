use string_reader::*;
use token::*;
use self::Token::*;

pub struct Tokenizer<'a> {
    reader: StringReader<'a>,
}

impl<'a> Tokenizer<'a> {
    pub fn new(src: &'a str) -> Self {
        Tokenizer {
            reader: StringReader::new(src)
        }
    }

    pub fn str_from_to(&self, start: BytePos, end: BytePos) -> &'a str {
        self.reader.str_from_to(start, end)
    }

    fn next_token(&mut self) -> Option<TokenAndPos<'a>> {
        while let Some(c) = self.reader.peek() {
            if !c.is_whitespace() {
                let pos = self.reader.pos();
                match c {
                    '*' => {
                        self.reader.next();
                        return Some(TokenAndPos::new(Asterisk, pos));
                    }
                    '(' => {
                        self.reader.next();
                        return Some(TokenAndPos::new(OpenParen, pos));
                    }
                    ')' => {
                        self.reader.next();
                        return Some(TokenAndPos::new(CloseParen, pos));
                    }
                    ',' => {
                        self.reader.next();
                        return Some(TokenAndPos::new(Comma, pos));
                    }
                    ':' => {
                        self.reader.next();
                        return Some(TokenAndPos::new(Colon, pos));
                    }
                    ';' => {
                        self.reader.next();
                        return Some(TokenAndPos::new(SemiColon, pos));
                    }
                    '-' => {
                        self.reader.next();
                        if let Some(next_ch) = self.reader.peek() {
                            if next_ch == '>' {
                                self.reader.next();
                                return Some(TokenAndPos::new(Arrow, pos));
                            } else {
                                panic!("invalid character: {:?}", c);
                            }
                        }
                    }
                    _ if c.is_alphabetic() => {
                        self.reader.advance_while(|c| c.is_alphanumeric() || c == '_');
                        let id = self.reader.str_from(pos);
                        return match id {
                            "pub" => Some(TokenAndPos::new(Pub, pos)),
                            "fn" => Some(TokenAndPos::new(Function, pos)),
                            "const" => Some(TokenAndPos::new(Const, pos)),
                            "mut" => Some(TokenAndPos::new(Mut, pos)),
                            _ => Some(TokenAndPos::new(Id(id), pos)),
                        };
                    },
                    _ => {
                        panic!("invalid character: {:?}", c);
                    }
                }
            }

            // default skip character
            self.reader.next();
        }
        
        None
    }
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = TokenAndPos<'a>;
    fn next(&mut self) -> Option<Self::Item> {
        self.next_token()
    }
}

#[test]
fn test_tokenizer() {
    let str = "fn blah(foo: i32) -> crap;";
    let mut tokens = Tokenizer::new(str);
    assert_eq!(tokens.next(), Some(TokenAndPos::new(Function,0)));
    assert_eq!(tokens.next(), Some(TokenAndPos::new(Id("blah"), 3)));
    assert_eq!(tokens.next(), Some(TokenAndPos::new(OpenParen, 7)));
    assert_eq!(tokens.next(), Some(TokenAndPos::new(Id("foo"), 8)));
    assert_eq!(tokens.next(), Some(TokenAndPos::new(Colon, 11)));
    assert_eq!(tokens.next(), Some(TokenAndPos::new(Id("i32"), 13)));
    assert_eq!(tokens.next(), Some(TokenAndPos::new(CloseParen, 16)));
    assert_eq!(tokens.next(), Some(TokenAndPos::new(Arrow, 18)));
    assert_eq!(tokens.next(), Some(TokenAndPos::new(Id("crap"), 21)));
    assert_eq!(tokens.next(), Some(TokenAndPos::new(SemiColon, 25)));
    assert_eq!(tokens.next(), None);
}
