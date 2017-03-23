extern crate fn_decl_parser;
extern crate argparse;
extern crate itertools;

use argparse::{ArgumentParser, StoreTrue, Store, StoreOption};
use std::fs::File;
use std::io::{self, Read};
use std::error::Error;
use itertools::Itertools;
use fn_decl_parser::*;

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
        
        println!(r#"  #[inline]
  fn {}({}){} {{
    (self.{})({})
  }}"#,
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
        
        println!(r#"  #[inline]
  fn {}({}){} {{
    unsafe {{ {}({}) }}
  }}"#,
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
