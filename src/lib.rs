use std::fmt::{self, Display, Formatter};

use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::quote;
use syn::{
    parse_macro_input, spanned::Spanned, Expr, ExprBlock, ExprGroup, ExprParen, ExprUnsafe, Ident,
    Lit, Stmt, Type,
};

enum Typename<'a> {
    // Int, string, bool, etc
    Ident(syn::Ident),
    Never,
    Tuple(Vec<Self>),
    Array(Box<Self>, ArrayLength<'a>),

    // A real type we got from the expression, perhaps from an `as` cast
    Type(&'a Type),

    // A reference to some other type. Always static.
    Reference(Box<Self>),

    // A slice of something
    Slice(Box<Self>),
}

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

#[derive(Clone, Copy)]
enum ArrayLength<'a> {
    Raw(usize),
    Expr(&'a Expr),
}

impl ArrayLength<'_> {
    fn into_tokens(self) -> proc_macro2::TokenStream {
        match self {
            ArrayLength::Raw(len) => quote! {#len},
            ArrayLength::Expr(expr) => quote! {#expr},
        }
    }
}

impl<'a> Typename<'a> {
    pub fn unit() -> Typename<'static> {
        Typename::Tuple(Vec::new())
    }

    pub fn primitive(name: &str) -> Typename<'static> {
        Typename::Ident(Ident::new(name, Span::call_site()))
    }

    pub fn str() -> Typename<'static> {
        Typename::Reference(Box::new(Self::primitive("str")))
    }

    pub fn bytes() -> Typename<'static> {
        Typename::Reference(Box::new(Typename::Slice(Box::new(Self::primitive("u8")))))
    }

    // TODO: better errors
    pub fn from_expr(expr: &'a Expr) -> Result<Self, Error> {
        let span = expr.span();
        let error = move |kind| Error {
            location: span,
            kind,
        };

        match expr {
            // The whole point of this crate:
            Expr::Lit(literal) => Ok(match literal.lit {
                Lit::Str(_) => Self::str(),
                Lit::ByteStr(_) => Self::bytes(),
                Lit::Byte(_) => Self::primitive("u8"),
                Lit::Char(_) => Self::primitive("char"),
                Lit::Int(ref i) => Self::primitive(match i.suffix() {
                    "" => "i32",
                    suffix => suffix,
                }),
                Lit::Float(ref f) => Self::primitive(match f.suffix() {
                    "" => "f64",
                    suffix => suffix,
                }),
                Lit::Bool(_) => Self::primitive("bool"),
                _ => return Err(error(ErrorKind::UnrecognizedLiteral)),
            }),

            // Fun extras:
            // Composite types: tuple, arrays, reference
            Expr::Array(array) => {
                // TODO: theoretically we could scan until we find a literal
                // and use that, to allow expressions like `[a, b, "hello"]`
                let inner_expr = array.elems.first().ok_or(error(ErrorKind::EmptyArray))?;
                let inner = Typename::from_expr(inner_expr)?;
                let len = array.elems.len();
                Ok(Typename::Array(Box::new(inner), ArrayLength::Raw(len)))
            }

            Expr::Repeat(array) => {
                let inner = Typename::from_expr(&*array.expr)?;
                let len = ArrayLength::Expr(&*array.len);
                Ok(Typename::Array(Box::new(inner), len))
            }

            Expr::Reference(reference) => Self::from_expr(&*reference.expr)
                .map(Box::new)
                .map(Self::Reference),

            Expr::Tuple(t) => t
                .elems
                .iter()
                .map(Self::from_expr)
                .collect::<Result<_, _>>()
                .map(Self::Tuple),

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
                    Some(expr) => Self::from_expr(expr),
                    None => Ok(Self::unit()),
                }
            }

            Expr::Group(ExprGroup { expr, .. }) | Expr::Paren(ExprParen { expr, .. }) => {
                Self::from_expr(&**expr)
            }

            // Control flow stuff has type `!`
            Expr::Return(_) | Expr::Break(_) | Expr::Continue(_) => Ok(Self::Never),

            // Casts are great cause they give us the literal type. Not really
            // clear why you're using this crate in this case, but we're not
            // here to judge.
            Expr::Cast(cast) => Ok(Self::Type(&*cast.ty)),

            // Assignments are valid expressions of unit type
            Expr::Assign(_) => Ok(Self::unit()),

            _ => Err(error(ErrorKind::UnsupportedExpression)),
        }
    }

    pub fn into_tokens(self) -> proc_macro2::TokenStream {
        match self {
            Typename::Ident(ident) => quote! {#ident},
            Typename::Never => quote! {!},
            Typename::Tuple(names) => {
                let names = names.into_iter().map(|name| name.into_tokens());
                quote! {( #(#names,)* )}
            }
            Typename::Array(name, len) => {
                let name = name.into_tokens();
                let len = len.into_tokens();
                quote! { [#name; #len] }
            }
            Typename::Type(ty) => quote! {#ty},
            Typename::Reference(inner) => {
                let inner = inner.into_tokens();
                quote! { &'static #inner }
            }
            Typename::Slice(inner) => {
                let inner = inner.into_tokens();
                quote! { [#inner] }
            }
        }
    }
}

#[proc_macro]
pub fn typeof_literal(expr: TokenStream) -> TokenStream {
    let expr = parse_macro_input!(expr as Expr);

    match Typename::from_expr(&expr) {
        Ok(name) => name.into_tokens(),
        Err(err) => syn::Error::new(err.location, err.kind).into_compile_error(),
    }
    .into()
}
