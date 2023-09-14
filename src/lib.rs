/*!
`typeof_literal` is a macro that can get the type of any literal or
composite-of-literals expression. It can be used to help with trait
implementations or function definitions that are generated by macros, where the
macro has access to some literal value but still needs to name the type in a
function return or trait associated type.

# Example

```
use typeof_literal::typeof_literal;

/// This macro creates a `const fn` called `$name` that returns `$value`.
macro_rules! maker {
    ($name:ident => $value:expr) => {
        const fn $name() -> typeof_literal!{$value} {
            $value
        }
    }
}

maker! {ten => 10i16}
let value: i16 = ten();
assert_eq!(value, 10);

maker! {hello_world => "Hello, World"}
assert_eq!(hello_world(), "Hello, World");

// It works on composite types
maker! {tuple => (123, [0u8; 2], [1i64, 2, 3, 4], "Hello")};
assert_eq!(tuple(), (123i32, [0, 0], [1, 2, 3, 4], "Hello"));

// It also works on common macro literal-ish expressions
maker! {concatenated => concat!(1, 2, 3, "abc")}
assert_eq!(concatenated(), "123abc");

// We support blocks and other nested expressions, and can use `as` to support
// *any* arbitrary expression
maker! {computed => {
    let x = 1;
    let y = 2;
    (x + y) as i32
}}
assert_eq!(computed(), 3);
```

In these examples, the `maker` macro is used to create a function that returns
a literal value. `typeof_literal` allows us to automatically know the type of
that literal, so that we can use it for the function's return type, instead of
forcing the caller to include it with something like
`maker!(int => 123 as i64)`.

Currently it only works on primitive literals and composites of literals. In
the future we may add support for simple binary operations like `1 + 2`, struct
literals, etc.
*/

use std::fmt::{self, Display, Formatter};

use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::{format_ident, quote};
use syn::{
    parse_macro_input, spanned::Spanned, Expr, ExprBlock, ExprCast, ExprGroup, ExprParen,
    ExprUnsafe, Lit, Stmt,
};

struct Error {
    location: Span,
    kind: ErrorKind,
}

enum ErrorKind {
    UnsupportedExpression,
    EmptyArray,
    UnrecognizedLiteral,
}

impl Display for ErrorKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match *self {
            ErrorKind::UnsupportedExpression => write!(
                f,
                "`typeof_literal` only works on literals and trivial composites of literals"
            ),
            ErrorKind::EmptyArray => write!(f, "can't determine the type of an empty array"),
            ErrorKind::UnrecognizedLiteral => {
                write!(f, "this is a literal but syn couldn't identify it")
            }
        }
    }
}

fn default_str<'a>(input: &'a str, default: &'a str) -> &'a str {
    match input {
        "" => default,
        input => input,
    }
}

fn typename(expr: &Expr) -> Result<proc_macro2::TokenStream, Error> {
    let span = expr.span();
    let error = move |kind| Error {
        location: span,
        kind,
    };

    match expr {
        // The whole point of this crate:
        Expr::Lit(literal) => Ok(match literal.lit {
            Lit::Str(_) => quote! {&'static str},
            Lit::ByteStr(_) => quote! {&'static [u8]},
            Lit::Byte(_) => quote! {u8},
            Lit::Char(_) => quote! {char},
            Lit::Int(ref i) => {
                let ty = format_ident!("{}", default_str(i.suffix(), "i32"));
                quote! {#ty}
            }
            Lit::Float(ref f) => {
                let ty = format_ident!("{}", default_str(f.suffix(), "f64"));
                quote! {#ty}
            }
            Lit::Bool(_) => quote! {bool},
            _ => return Err(error(ErrorKind::UnrecognizedLiteral)),
        }),

        // Fun extras:
        // Composite types: tuple, arrays, reference
        Expr::Array(array) => {
            let inner_expr = array.elems.first().ok_or(error(ErrorKind::EmptyArray))?;

            let inner = typename(inner_expr)?;
            let len = array.elems.len();

            Ok(quote! {[#inner; #len]})
        }

        Expr::Repeat(array) => {
            let inner = typename(&*array.expr)?;
            let len = &array.len;
            Ok(quote! {[#inner; #len]})
        }

        Expr::Reference(reference) => {
            let inner = typename(&*reference.expr)?;
            Ok(quote! {&'static #inner})
        }

        Expr::Tuple(t) => {
            let types = t
                .elems
                .iter()
                .map(typename)
                .collect::<Result<Vec<_>, _>>()?;

            Ok(quote! {(#(#types,)*)})
        }

        // Block-like things; just look to see if there's a literal in
        // the tail position
        Expr::Unsafe(ExprUnsafe { block, .. }) | Expr::Block(ExprBlock { block, .. }) => {
            // Find the expression without a semicolon. Don't bother trying
            // to find `return` expressions.
            let tail = block.stmts.iter().find_map(|statement| match statement {
                Stmt::Expr(expr, None) => Some(expr),
                _ => None,
            });

            match tail {
                Some(expr) => typename(expr),
                None => Ok(quote! {()}),
            }
        }

        Expr::Group(ExprGroup { expr, .. }) | Expr::Paren(ExprParen { expr, .. }) => typename(expr),

        // Control flow stuff has type `!`
        Expr::Return(_) | Expr::Break(_) | Expr::Continue(_) => Ok(quote! {!}),

        // Casts are great cause they give us the literal type. Not really
        // clear why you're using this crate in this case, but we're not
        // here to judge.
        Expr::Cast(ExprCast { ty, .. }) => Ok(quote! {#ty}),

        // Assignments are valid expressions of unit type
        Expr::Assign(_) => Ok(quote! {()}),

        Expr::Macro(mac) => match mac.mac.path.get_ident() {
            Some(ident)
                if ident == "concat"
                    || ident == "stringify"
                    || ident == "env"
                    || ident == "file"
                    || ident == "include_file" =>
            {
                Ok(quote! {&'static str})
            }
            Some(ident) if ident == "cfg" => Ok(quote! {bool}),
            Some(ident) if ident == "option_env" => Ok(quote! {Option<&'static str>}),
            Some(ident) if ident == "line" || ident == "column" => Ok(quote! {u32}),
            Some(ident) if ident == "include_bytes" => Ok(quote! {&'static [u8]}),

            _ => Err(error(ErrorKind::UnsupportedExpression)),
        },

        _ => Err(error(ErrorKind::UnsupportedExpression)),
    }
}

/**
Macro that returns the *type* of any literal expression. See the
[module docs][crate] for details.
*/
#[proc_macro]
pub fn typeof_literal(expr: TokenStream) -> TokenStream {
    let expr = parse_macro_input!(expr as Expr);

    match typename(&expr) {
        Ok(name) => name,
        Err(err) => syn::Error::new(err.location, err.kind).into_compile_error(),
    }
    .into()
}
