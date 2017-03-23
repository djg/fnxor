extern crate argparse;
extern crate itertools;

use argparse::{ArgumentParser, StoreTrue, Store, StoreOption};
use std::usize;
use std::fs::File;
use std::io::{self, Read};
use std::error::Error;
use itertools::Itertools;

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

use Token::*;

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

fn char_at(s: &str, offset: usize) -> char {
    s[offset..].chars().next().unwrap()
}

type BytePos = usize;

struct StringReader<'a> {
    pos: BytePos,
    input: &'a str,
}

impl<'a> StringReader<'a> {
    pub fn new(s: &'a str) -> StringReader<'a> {
        StringReader {
            pos: 0,
            input: s,
        }
    }

    pub fn pos(&self) -> BytePos { self.pos }
    
    pub fn next(&mut self) {
        let pos = self.pos;
        if pos < self.input.len() {
            let new_ch = char_at(self.input, pos);
            let new_ch_len = new_ch.len_utf8();
            self.pos += new_ch_len;
        }
    }

    pub fn peek(&self) -> Option<char> {
        if self.pos < self.input.len() {
            Some(char_at(self.input, self.pos))
        } else {
            None
        }
    }

    pub fn advance_while<P>(&mut self, pred: P) where P: Fn(char) -> bool {
        while let Some(c) = self.peek() {
            if !pred(c) {
                return;
            }
            self.next();
        }
    }
    
    pub fn str_from(&self, start: BytePos) -> &'a str {
        &self.input[start..self.pos]
    }

    pub fn str_from_to(&self, start: BytePos, end: BytePos) -> &'a str {
        &self.input[start..end]
    }
}

struct Tokenizer<'a> {
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

#[derive(Debug,PartialEq)]
struct FnDecl<'a> {
    pub name: &'a str,
    pub args: Vec<(&'a str, &'a str)>,
    pub ret: Option<&'a str>
}

struct FnDeclIterator<'a> {
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

fn format_ret(ret: &Option<&str>, result: &mut String) {
   *result = match ret {
        &Some(ret_type) => format!(" -> {}", ret_type),
        &None => format!("")
    };
}

fn format_args(args: &Vec<(&str, &str)>, with_self: bool, result: &mut String) {
    *result = match args.len() {
        0 => format!("{}", if with_self { "&self" } else { "" }),
        _ => format!("{}{}",
                     if with_self { "&self, " } else { "" },
                     args.iter().format_with(", ", |arg, f| f(&format_args!("{}: {}", arg.0, arg.1))))
    }
}

fn format_call(args: &Vec<(&str, &str)>, result: &mut String) {
    *result = format!("{}", args.iter().format_with(", ", |arg, f| f(&format_args!("{}", arg.0))));
}

fn format_call_types(args: &Vec<(&str, &str)>, result: &mut String) {
    *result = format!("{}", args.iter().format_with(", ", |arg, f| f(&format_args!("{}", arg.1))));
}

fn write_header() {
    println!("{}", r#"// This code is generated by fuxor. (https://github.com/djg/fuxor)

use libc::{{c_char,c_void}};
use libc::{{dlclose,dlopen,dlsym,RTLD_LAZY}};

macro_rules! cstr {{
  ($x:expr) => { concat!($x, "\0").as_bytes().as_ptr() as *const c_char }}
}}
"#);
}

fn write_dynamic_struct(decls: &Vec<FnDecl>) {
    println!("struct DynamicFoo {{");
    let mut call = String::with_capacity(100);
    let mut ret = String::with_capacity(80);

    for decl in decls {
        format_call_types(&decl.args, &mut call);
        format_ret(&decl.ret, &mut ret);
        
        println!("  {}: fn({}){},", decl.name, call, ret);
    }
    
    println!("  __lib: *mut c_void\n}}\n");
}

fn write_dynamic_impl(decls: &Vec<FnDecl>) {
    println!(r#"impl DynamicFoo {{
  pub fn open() -> Option<DynamicFoo> {{
    let h = unsafe {{ dlopen(foo!({}), RTLD_LAZY) }};
    if h.is_null() {{ return None; }}
 
    let r = DynamicMath {{"#,
    r#""libfoo""#);

    for decl in decls {
        println!(r#"      {}: unsafe {{
        let fp = dlsym(h, foo!("{}"));
        if fp.is_null() {{ return None; }}
        ::std::mem::transmute(fp)
      }},"#,
        decl.name, decl.name);
    }
    
    println!(r#"      __lib: h
    }};
    Some(r)
  }}
}}

impl ::std::ops::Drop for DynamicFoo {{
  #[inline]
  fn drop(&mut self) {{
    unsafe {{ dlclose(self.__lib); }}
  }}
}}
"#);
}

fn write_dynamic_traits(decls: &Vec<FnDecl>) {
    println!("impl FooFns for DynamicFoo {{");

    let mut args = String::with_capacity(100);
    let mut call = String::with_capacity(100);
    let mut ret = String::with_capacity(80);

    for decl in decls {
        format_args(&decl.args, true, &mut args);
        format_ret(&decl.ret, &mut ret);
        format_call(&decl.args, &mut call);
        
        println!("  #[inline]\n  fn {}({}){} {{\n    (self.{})({})\n  }}",
                 decl.name, args, ret, decl.name, call);
    }
    
    println!("}}\n");
}

fn write_extern(decls: &Vec<FnDecl>) {
    println!(r#"extern "C" {{"#);
    let mut args = String::with_capacity(100);
    let mut ret = String::with_capacity(80);
    
    for decl in decls {
        format_args(&decl.args, false, &mut args);
        format_ret(&decl.ret, &mut ret);
        println!("  {}({}){};", decl.name, args, ret);
    }
    println!("}}\n");
}

fn write_static_traits(decls: &Vec<FnDecl>) {
    println!("impl FooFns for StaticFoo {{");
    let mut args = String::with_capacity(100);
    let mut call = String::with_capacity(100);
    let mut ret = String::with_capacity(80);

    for decl in decls {
        format_args(&decl.args, true, &mut args);
        format_ret(&decl.ret, &mut ret);
        format_call(&decl.args, &mut call);
        
        println!("  #[inline]\n  fn {}({}){} {{\n    unsafe {{ {}({}) }}\n  }}",
                 decl.name, args, ret, decl.name, call);
    }

    println!("}}\n");
}

fn process(src: &str) {
    let fn_decl_parser = FnDeclIterator::new(src);
    let fn_decls: Vec<FnDecl> = fn_decl_parser.collect();

    write_header();
    write_extern(&fn_decls);
    write_static_traits(&fn_decls);
    write_dynamic_struct(&fn_decls);
    write_dynamic_impl(&fn_decls);
    write_dynamic_traits(&fn_decls);
}

fn main() {
    let mut verbose = false;
    let mut prefix = "foo".to_string();
    let mut input_path: Option<String> = None;
    {
        let mut ap = ArgumentParser::new();
        ap.set_description("Process rust fn declaration into static and dynamic library loading.");
        ap.refer(&mut verbose)
            .add_option(&["-v", "--verbose"], StoreTrue, "");
        ap.refer(&mut prefix)
            .add_option(&["--prefix"], Store, "Prefix for library name");
        ap.refer(&mut input_path)
            .add_option(&["--input"], StoreOption, "Path of input. Defaults to stdin");

        ap.parse_args_or_exit();
    }

    let mut source = String::new();
    match input_path {
        Some(ref s) => {
            let mut file = match File::open(s) {
                Err(why) => panic!("couldn't open {}: {}", s, why.description()),
                Ok(file) => {
                    file
                }
            };

            match file.read_to_string(&mut source) {
                Err(why) => panic!("couldn't read file: {}", why.description()),
                _ => {}
            }
        }
        None => {
            let mut buffer = String::new();
            let stdin = io::stdin();
            let mut handle = stdin.lock();
            match handle.read_to_string(&mut buffer) {
                Err(why) => panic!("couldn't read stdin: {}", why.description()),
                _ => {}
            }
        }
    }

    process(source.as_str());
}

#[test]
fn test_string_reader() {
    let str = "fn blah(foo: i32) -> crap;";
    let mut reader = StringReader::new(str);
    assert_eq!(reader.peek(), Some('f')); reader.next();
    assert_eq!(reader.peek(), Some('n'));
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
