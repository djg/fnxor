use itertools::Itertools;
use fn_decl_parser::*;
use std::path::{Path, PathBuf};
use io::{self,Read};
use std::fs::File;

enum Input {
    Stdin,
    File(PathBuf),
}

#[derive(Clone)]
enum Output {
    Stdout,
    File(PathBuf),
}

pub struct Generator {
    prefix: String,
    input: Input,
    output: Output,
    verbose: bool,    
}

impl Generator {
    pub fn new() -> Generator {
        Generator {
            prefix: String::new(),
            input: Input::Stdin,
            output: Output::Stdout,
            verbose: false
        }
    }
    
    pub fn with_input<P>(&mut self, path: P) -> &mut Self
        where P: AsRef<Path>
    {
        self.input = Input::File(path.as_ref().to_path_buf());
        self
    }
    
    pub fn with_output<P>(&mut self, path: P) -> &mut Self
        where P: AsRef<Path>
    {
        self.output = Output::File(path.as_ref().to_path_buf());
        self
    }

    pub fn prefix<'a, S>(&'a mut self, prefix: S) -> &'a mut Self
        where S: AsRef<str>
    {
        self.prefix = prefix.as_ref().to_string();
        self
    }
    
    pub fn verbose<'a>(&'a mut self, verbose: bool) -> &'a mut Self
    {
        self.verbose = verbose;
        self
    }
    
    pub fn build(&mut self) -> io::Result<()>  {

        let mut source = String::new();
        match self.input {
            Input::File(ref s) => {
                let mut file = try!(File::open(s));
                try!(file.read_to_string(&mut source));
            }
            Input::Stdin => {
                try!(io::stdin().read_to_string(&mut source));
            }
        }
        let fn_decls = FnDeclIterator::new(&source).collect();

        let output = self.output.clone();
        match output {
            Output::File(ref s) => {
                let mut file = try!(File::create(s));
                self.process(&mut file, fn_decls)
            }
            Output::Stdout => {
                self.process(&mut io::stdout(), fn_decls)
            }
        }
    }

    fn process<'a>(&self, output: &mut io::Write, fn_decls: Vec<FnDecl<'a>>) -> io::Result<()> {
        try!(self.write_header(output));
        try!(self.write_static_mod(output, &fn_decls));
        self.write_dynamic_mod(output, &fn_decls)
    }

    fn write_static_mod(&self, output: &mut io::Write, fn_decls: &Vec<FnDecl>) -> io::Result<()> {
        try!(write!(output, r#"#[cfg(feature = "static-link")]
mod static_fns {{
    use libc::{{c_char,c_int,c_void}};
    use super::*;

    extern "C" {{
"#));
        let mut args = String::with_capacity(100);
        let mut ret = String::with_capacity(80);
        
        for decl in fn_decls {
            format_args(&decl.args, &mut args);
            format_ret(&decl.ret, &mut ret);
            try!(writeln!(output, "        pub fn {}({}){};", decl.name, args, ret));
        }
        try!(write!(output, r#"    }}
}}

#[cfg(feature = "static-link")]
pub use self::static_fns::*;

"#));
        Ok(())
    }

    fn write_dynamic_mod(&self, output: &mut io::Write, fn_decls: &Vec<FnDecl>) -> io::Result<()> {
        try!(write!(output, r#"#[cfg(feature = "dynamic-link")]
mod dynamic_fns {{
    use libc::{{c_char,c_int,c_void}};
    use libc::{{dlclose,dlopen,dlsym,RTLD_LAZY}};
    use super::*;

"#));
        try!(self.write_lib_loader(output, fn_decls));
        try!(self.write_shims(output, fn_decls));
        try!(write!(output, r#"}}

#[cfg(feature = "dynamic-link")]
pub use self::dynamic_fns::*;
"#));
        Ok(())
    }
    
    fn write_header(&self, output: &mut io::Write) -> io::Result<()> {
        writeln!(output, "{}", r#"// This code is generated.

use super::*;

macro_rules! cstr {
  ($x:expr) => { concat!($x, "\0").as_bytes().as_ptr() as *const c_char }
}
"#)
    }

    fn write_lib_loader(&self, output: &mut io::Write, fn_decls: &Vec<FnDecl>) -> io::Result<()> {
        try!(write!(output, r#"    pub struct LibLoader {{
        _lib: *mut c_void
    }}

    impl LibLoader {{
        pub unsafe fn open() -> Option<LibLoader> {{
            let h = dlopen(cstr!("lib"), RTLD_LAZY);
            if h.is_null() {{ return None; }}

"#));
        for decl in fn_decls {
            try!(writeln!(output, r#"            {} = {{
                let fp = dlsym(h, cstr!("{}"));
                if fp.is_null() {{ return None; }}
                fp
            }};"#,
                          decl.name.to_uppercase(), decl.name));
        }
    
        try!(write!(output, r#"
            Some(LibLoader {{
                _lib: h
            }})
        }}
    }}

    impl ::std::ops::Drop for LibLoader {{
        #[inline]
        fn drop(&mut self) {{
            unsafe {{ dlclose(self._lib); }}
        }}
    }}

"#));
        Ok(())
    }

    fn write_shims(&self, output: &mut io::Write, fn_decls: &Vec<FnDecl>) -> io::Result<()> {
        let mut args = String::with_capacity(100);
        let mut call = String::with_capacity(100);
        let mut call_types = String::with_capacity(100);
        let mut ret = String::with_capacity(80);

        for decl in fn_decls {
            format_args(&decl.args, &mut args);
            format_ret(&decl.ret, &mut ret);
            format_call(&decl.args, &mut call);
            format_call_types(&decl.args, &mut call_types);
            
            try!(writeln!(output, r#"    static mut {}: *mut ::libc::c_void = 0 as *mut _;
    #[inline]
    pub unsafe fn {}({}){} {{
        (::std::mem::transmute::<_, extern fn({}){}>({}))({})
    }}
"#,
            decl.name.to_uppercase(), decl.name, args, ret, call_types, ret, decl.name.to_uppercase(), call));
        }
        
        Ok(())
    }
}

fn format_ret(ret: &Option<&str>, result: &mut String) {
   *result = match ret {
        &Some(ret_type) => format!(" -> {}", ret_type),
        &None => format!("")
    };
}

fn format_args(args: &Vec<(&str, &str)>, result: &mut String) {
    *result = match args.len() {
        0 => "".to_string(),
        _ => format!("{}", args.iter().format_with(", ", |arg, f| f(&format_args!("{}: {}", arg.0, arg.1))))
    }
}

fn format_call(args: &Vec<(&str, &str)>, result: &mut String) {
    *result = format!("{}", args.iter().format_with(", ", |arg, f| f(&format_args!("{}", arg.0))));
}

fn format_call_types(args: &Vec<(&str, &str)>, result: &mut String) {
    *result = format!("{}", args.iter().format_with(", ", |arg, f| f(&format_args!("{}", arg.1))));
}
