use string_reader::BytePos;
use token::*;
use tokenizer::*;

use self::Token::*;

#[derive(Debug,PartialEq)]
pub struct FnDecl<'a> {
    pub name: &'a str,
    pub args: Vec<(&'a str, &'a str)>,
    pub ret: Option<&'a str>
}

pub struct FnDeclIterator<'a> {
    next_tok: Option<TokenAndPos<'a>>,
    tokenizer: Tokenizer<'a>,
}

macro_rules! t {
    ($e: expr) => {
        match $e {
            Some(x) => x,
            None => { return None; }
        }
    }
}

impl<'a> FnDeclIterator<'a> {
    pub fn new(src: &'a str) -> FnDeclIterator<'a> {
        let mut it = FnDeclIterator {
            next_tok: None,
            tokenizer: Tokenizer::new(src)
        };
        it.bump();
        it
    }

    fn bump(&mut self) {
        self.next_tok = self.tokenizer.next();
        //println!("next_tok = {:?}", self.next_tok);
    }

    #[doc(hidden)]
    fn _token_is<P>(&self, pred: P) -> bool
        where P: FnOnce(&Token<'a>) -> bool
    {
        match self.next_tok.as_ref() {
            Some(t) => pred(&t.tok),
            None => false,
        }
    }

    fn token_is(&self, tok: Token<'a>) -> bool {
        self._token_is(|t| t == &tok)
    }

    fn token_is_id(&self) -> Option<(&'a str, BytePos)> {
        match self.next_tok.as_ref() {
            Some(&TokenAndPos{ tok: Id(s), pos: p }) => Some((s, p)),
            _ => None,
        }
    }
    
    fn accept_if<P>(&mut self, pred: P) -> Option<()>
        where P: FnOnce(&Token<'a>) -> bool
    {
        let accept = self._token_is(pred);

        if accept {
            self.bump();
            Some(())
        } else {
            None
        }
    }

    fn accept(&mut self, tok: Token<'a>) -> Option<()> {
        self.accept_if(|t| t == &tok)
    }
    
    fn accept_id(&mut self) -> Option<&'a str> {
        match self.next_tok {
            Some(TokenAndPos{ tok: Token::Id(id), pos: _ }) => { self.bump(); Some(id) }
            _ => None
        }
    }

    fn accept_type(&mut self) -> Option<&'a str> {
        if self.token_is(Token::Asterisk) {
            // pointer type
            let start = t!(self.next_tok.as_ref().map_or(None, |t| Some(t.pos)));
            t!(self.accept(Token::Asterisk));
            t!(self.accept_if(|t| t == &Token::Mut || t == &Token::Const));
            let (id, pos) = t!(self.token_is_id());
            let end = pos + id.len();
            let s = self.tokenizer.str_from_to(start, end);
            let _ = t!(self.accept_id());
            Some(s)
        } else {
            self.accept_id()
        }
    }
}

impl<'a> Iterator for FnDeclIterator<'a> {
    type Item = FnDecl<'a>;
    
    fn next(&mut self) -> Option<FnDecl<'a>> {
        if self.token_is(Token::Pub) {
            t!(self.accept(Token::Pub));
        }
        t!(self.accept(Token::Function));
        let fn_name = t!(self.accept_id());
        t!(self.accept(Token::OpenParen));
        let mut args = vec![];
        while !self.token_is(Token::CloseParen) {
            if args.len() > 0 {
                t!(self.accept(Token::Comma))
            }
            
            let arg_name = t!(self.accept_id());
            t!(self.accept(Token::Colon));
            
            let arg_type = t!(self.accept_type());
            
            args.push((arg_name, arg_type));
        }
        t!(self.accept(Token::CloseParen));
        let ret_type = if self.token_is(Token::Arrow) {
            t!(self.accept(Token::Arrow));
            let r = t!(self.accept_type());
            Some(r)
        } else {
            None
        };
        t!(self.accept(Token::SemiColon));

        Some(FnDecl {
            name: fn_name,
            args: args,
            ret: ret_type
        })
    }
}

#[test]
fn test_fn_decl_iterator_arg_ret() {
    let str = "fn blah(foo: i32) -> crap;";
    let mut fn_decl_parser = FnDeclIterator::new(str);

    assert_eq!(fn_decl_parser.next(), Some(
        FnDecl{
            name: "blah",
            args: vec![("foo", "i32")],
            ret: Some("crap")
        }
    ));
    assert_eq!(fn_decl_parser.next(), None);
}

#[test]
fn test_fn_decl_iterator_arg_void() {
    let str = "fn blah(foo: i32);";
    let mut fn_decl_parser = FnDeclIterator::new(str);

    assert_eq!(fn_decl_parser.next(), Some(
        FnDecl{
            name: "blah",
            args: vec![("foo", "i32")],
            ret: None
        }
    ));
    assert_eq!(fn_decl_parser.next(), None);
}

#[test]
fn test_fn_decl_void_void() {
    let str = "fn foo();";
    let mut fn_decl_parser = FnDeclIterator::new(str);

    assert_eq!(fn_decl_parser.next(), Some(
        FnDecl{
            name: "foo",
            args: vec![],
            ret: None
        }
    ));
    assert_eq!(fn_decl_parser.next(), None);
}

#[test]
fn test_fn_decl_void_ret() {
    let str = "fn foo() -> i32;";
    let mut fn_decl_parser = FnDeclIterator::new(str);

    assert_eq!(fn_decl_parser.next(), Some(
        FnDecl{
            name: "foo",
            args: vec![],
            ret: Some("i32")
        }
    ));
    assert_eq!(fn_decl_parser.next(), None);
}

#[test]
fn test_fn_decl_multi_args() {
    let str = "fn foo(a: i32, b: f32);";
    let mut fn_decl_parser = FnDeclIterator::new(str);

    assert_eq!(fn_decl_parser.next(), Some(
        FnDecl{
            name: "foo",
            args: vec![("a", "i32"), ("b", "f32")],
            ret: None
        }
    ));
    assert_eq!(fn_decl_parser.next(), None);
}

#[test]
fn test_fn_decl_with_ptrs() {
    let str = "fn foo(ptr: *mut bar) -> *const c_char;";
    let mut fn_decl_parser = FnDeclIterator::new(str);

        assert_eq!(fn_decl_parser.next(), Some(
        FnDecl{
            name: "foo",
            args: vec![("ptr", "*mut bar")],
            ret: Some("*const c_char")
        }
    ));
    assert_eq!(fn_decl_parser.next(), None);
}

#[test]
fn test_fn_decl_with_newline() {
    let str = "fn foo(ptr: i32,
baz: bar);";
    let mut fn_decl_parser = FnDeclIterator::new(str);

        assert_eq!(fn_decl_parser.next(), Some(
        FnDecl{
            name: "foo",
            args: vec![("ptr", "i32"), ("baz", "bar")],
            ret: None
        }
    ));
    assert_eq!(fn_decl_parser.next(), None);
}
