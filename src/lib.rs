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

        _ => Err(error(ErrorKind::UnsupportedExpression)),
    }
}

#[proc_macro]
pub fn typeof_literal(expr: TokenStream) -> TokenStream {
    let expr = parse_macro_input!(expr as Expr);

    match typename(&expr) {
        Ok(name) => name,
        Err(err) => syn::Error::new(err.location, err.kind).into_compile_error(),
    }
    .into()
}
