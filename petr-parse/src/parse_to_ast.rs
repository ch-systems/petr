use std::rc::Rc;

// using this crate's Parser, parse an AST.
use petr_ast::*;
use petr_utils::{Identifier, Path, SpannedItem};

use crate::{
    parser::{Parse, ParseErrorKind, Token},
    Parser,
};

impl Parse for FunctionCall {
    fn parse(p: &mut Parser) -> Option<Self> {
        p.with_help("while parsing function call", |p| -> Option<Self> {
            p.token(Token::Tilde)?;
            let func_name = p.parse()?;
            // optionally, args can be in parens to resolve ambiguity
            // like if they're in a list
            let open = p.try_token(Token::OpenParen);
            let args = p.sequence_zero_or_more(Token::Comma)?;
            if open.is_some() {
                p.token(Token::CloseParen)?;
            }
            Some(Self {
                func_name,
                args: args.into_boxed_slice(),
                args_were_parenthesized: open.is_some(),
            })
        })
    }
}

impl Parse for Path {
    fn parse(p: &mut Parser) -> Option<Self> {
        p.with_help("while parsing path", |p| -> Option<Self> {
            let identifiers = p.sequence_one_or_more(Token::Dot)?;
            Some(Path {
                identifiers: identifiers.into_boxed_slice(),
            })
        })
    }
}

impl Parse for TypeDeclaration {
    fn parse(p: &mut Parser) -> Option<Self> {
        p.with_help("while parsing type declaration", |p| -> Option<Self> {
            let tok = p.one_of([Token::TypeKeyword, Token::ExportTypeKeyword])?;
            let visibility = match tok.item() {
                Token::TypeKeyword => Visibility::Local,
                Token::ExportTypeKeyword => Visibility::Exported,
                _ => unreachable!(),
            };
            let name = p.parse()?;
            if *p.peek().item() != Token::Equals {
                // if there's no equals, then this is a type with no variants
                return Some(Self {
                    name,
                    variants: vec![].into_boxed_slice(),
                    visibility,
                });
            }
            p.token(Token::Equals)?;
            let variants = p.sequence(Token::Pipe)?;
            Some(Self {
                name,
                variants: variants.into_boxed_slice(),
                visibility,
            })
        })
    }
}

impl Parse for TypeVariant {
    fn parse(p: &mut Parser) -> Option<Self> {
        p.with_help("while parsing type variant", |p| -> Option<Self> {
            let mut buf = vec![];
            let name = p.parse()?;
            loop {
                let peek = *p.peek().item();
                if peek == Token::Identifier {
                    let span = p.span();
                    let field_name = p.parse()?;
                    let field = p.parse()?;
                    buf.push(span.with_item(TypeField {
                        name: field_name,
                        ty:   field,
                    }));
                } else {
                    break;
                }
            }
            Some(Self {
                name,
                fields: buf.into_boxed_slice(),
            })
        })
    }
}

impl Parse for AstNode {
    fn parse(p: &mut Parser) -> Option<Self> {
        p.with_help("while parsing AST node", |p| match p.peek().item() {
            Token::FunctionKeyword | Token::ExportFunctionKeyword => Some(AstNode::FunctionDeclaration(p.parse()?)),
            Token::TypeKeyword | Token::ExportTypeKeyword => Some(AstNode::TypeDeclaration(p.parse()?)),
            Token::Eof | Token::NewFile(..) => None,
            Token::Import | Token::Export => Some(AstNode::ImportStatement(p.parse()?)),
            a => {
                let span = p.peek().span();
                p.push_error(span.with_item(ParseErrorKind::ExpectedOneOf(
                    vec![Token::FunctionKeyword, Token::TypeKeyword, Token::Eof],
                    *a,
                )));
                None
            },
        })
    }
}

impl Parse for ImportStatement {
    fn parse(p: &mut Parser) -> Option<Self> {
        p.with_help("while parsing import or export statement", |p| -> Option<Self> {
            let tok = p.one_of([Token::Import, Token::Export])?;
            let visibility = match tok.item() {
                Token::Import => Visibility::Local,
                Token::Export => Visibility::Exported,
                _ => unreachable!(),
            };
            let path: Vec<Identifier> = p.sequence_one_or_more(Token::Dot)?;
            let alias = if p.try_token(Token::As).is_some() { Some(p.parse()?) } else { None };
            Some(Self {
                path: Path::new(path),
                visibility,
                alias,
            })
        })
    }
}

impl Parse for FunctionDeclaration {
    fn parse(p: &mut Parser) -> Option<Self> {
        p.with_help("while parsing function declaration", |p| -> Option<Self> {
            let tok = p.one_of([Token::FunctionKeyword, Token::ExportFunctionKeyword])?;
            let visibility = match tok.item() {
                Token::FunctionKeyword => Visibility::Local,
                Token::ExportFunctionKeyword => Visibility::Exported,
                _ => unreachable!(),
            };
            let name: Identifier = p.parse()?;
            p.token(Token::OpenParen)?;
            let parameters = if p.try_token(Token::CloseParen).is_some() {
                vec![].into_boxed_slice()
            } else {
                let seq = p.sequence(Token::Comma)?.into_boxed_slice();
                p.token(Token::CloseParen)?;
                seq
            };
            p.token(Token::ReturnsKeyword)?;
            let return_type = p.parse()?;
            let body = p.parse()?;
            Some(Self {
                name,
                parameters,
                return_type,
                body,
                visibility,
            })
        })
    }
}
impl Parse for Literal {
    fn parse(p: &mut Parser) -> Option<Self> {
        let tok = p.advance();
        match tok.item() {
            Token::Integer => Some(Literal::Integer(p.slice().parse().expect("lexer should have verified this"))),
            Token::String => Some(Literal::String(Rc::from(&p.slice()[1..p.slice().len() - 1]))),
            Token::True => Some(Literal::Boolean(true)),
            Token::False => Some(Literal::Boolean(false)),
            _ => {
                p.push_error(p.span().with_item(ParseErrorKind::ExpectedToken(Token::Integer, *tok.item())));
                None
            },
        }
    }
}
impl Parse for FunctionParameter {
    fn parse(p: &mut Parser) -> Option<Self> {
        p.with_help("while parsing function parameter", |p| -> Option<Self> {
            let name: Identifier = p.parse()?;
            p.one_of([Token::InKeyword, Token::IsInSymbol])?;
            let ty: Ty = p.parse()?;
            Some(FunctionParameter { name, ty })
        })
    }
}

impl Parse for Ty {
    // TODO types are not just idents,
    // they can be more than that
    fn parse(p: &mut Parser) -> Option<Self> {
        p.with_help("while parsing type", |p| -> Option<Self> {
            p.token(Token::TyMarker)?;
            let next: Identifier = p.parse()?;
            let ty = match p.slice() {
                "int" => Ty::Int,
                "bool" => Ty::Bool,
                "string" => Ty::String,
                "unit" => Ty::Unit,
                _ => Ty::Named(next),
            };

            Some(ty)
        })
    }
}
impl Parse for Operator {
    fn parse(p: &mut Parser) -> Option<Self> {
        let tok = p.advance();
        match tok.item() {
            Token::Plus => Some(Operator::Plus),
            Token::Minus => Some(Operator::Minus),
            Token::Star => Some(Operator::Star),
            Token::Slash => Some(Operator::Slash),
            _ => {
                p.push_error(p.span().with_item(ParseErrorKind::ExpectedOneOf(
                    vec![Token::Plus, Token::Minus, Token::Star, Token::Slash],
                    *tok.item(),
                )));
                None
            },
        }
    }
}
impl Parse for Identifier {
    fn parse(p: &mut Parser) -> Option<Self> {
        let identifier = p.advance();
        if *identifier.item() != Token::Identifier {
            p.push_error(p.span().with_item(ParseErrorKind::ExpectedIdentifier(p.slice().to_string())));
        }
        let span = p.span();
        let slice = Rc::from(p.slice());
        let id = p.intern(slice);
        Some(Identifier { id, span })
    }
}

impl Parse for Comment {
    fn parse(p: &mut Parser) -> Option<Self> {
        // because it matched in the lexer, we know the first two and last two chars
        // are {- and -}, so we slice those out.
        let slice = p.slice();
        let content = &slice[2..slice.len() - 2];
        let content = Rc::from(content.trim());
        Some(Comment::new(content))
    }
}

impl<T> Parse for Commented<T>
where
    T: Parse,
{
    fn parse(p: &mut crate::parser::Parser) -> Option<Self> {
        let item: T = p.parse()?;
        let comments = p.drain_comments();
        Some(Commented::new(item, comments))
    }
}

impl Parse for Expression {
    fn parse(p: &mut Parser) -> Option<Self> {
        p.with_help("while parsing expression", |p| -> Option<Self> {
            // TODO Parser "map" function which takes a list of tokens and their corresponding
            // parsers, which can auto-populate the ExpectedOneOf error
            match p.peek().item() {
                item if item.is_operator() => {
                    let op: SpannedItem<Operator> = p.parse()?;
                    // parse prefix notation operator expression
                    let lhs: SpannedItem<Expression> = p.parse()?;
                    let rhs: SpannedItem<Expression> = p.parse()?;
                    Some(Expression::Operator(Box::new(OperatorExpression { lhs, rhs, op })))
                },
                Token::Identifier => Some(Expression::Variable(p.parse()?)),
                Token::OpenBracket => Some(Expression::List(p.parse()?)),
                Token::Tilde => Some(Expression::FunctionCall(p.parse()?)),
                Token::True | Token::False | Token::String | Token::Integer => Some(Expression::Literal(p.parse()?)),
                Token::Intrinsic => Some(Expression::IntrinsicCall(p.parse()?)),
                Token::Let => Some(Expression::Binding(p.parse()?)),
                otherwise => {
                    p.push_error(p.span().with_item(ParseErrorKind::ExpectedOneOf(
                        vec![
                            Token::Identifier,
                            Token::OpenBracket,
                            Token::Tilde,
                            Token::True,
                            Token::False,
                            Token::String,
                            Token::Integer,
                            Token::Intrinsic,
                            Token::Let,
                        ],
                        *otherwise,
                    )));
                    None
                },
            }
        })
    }
}

impl Parse for ExpressionWithBindings {
    /// parse an expression that is prefaced with symbol bindings
    fn parse(p: &mut Parser) -> Option<Self> {
        p.token(Token::Let)?;

        let bindings: Vec<Binding> = p.sequence_one_or_more(Token::Comma)?;
        let expression = p.parse()?;

        Some(ExpressionWithBindings {
            bindings,
            expression: Box::new(expression),
            expr_id: p.new_expr_id(),
        })
    }
}

impl Parse for Binding {
    fn parse(p: &mut Parser) -> Option<Self> {
        p.with_help("while parsing let binding", |p| {
            let name: Identifier = p.parse()?;
            p.token(Token::Equals)?;
            let expr = p.parse()?;
            Some(Binding { name, val: expr })
        })
    }
}

impl Parse for IntrinsicCall {
    fn parse(p: &mut Parser) -> Option<Self> {
        p.with_help("while parsing intrinsic call", |p| -> Option<Self> {
            let name = p.slice().to_string();
            let intrinsic = match &name[1..] {
                "puts" => Intrinsic::Puts,
                "add" => Intrinsic::Add,
                "subtract" => Intrinsic::Subtract,
                "multiply" => Intrinsic::Multiply,
                "divide" => Intrinsic::Divide,
                "malloc" => Intrinsic::Malloc,
                a => todo!("unrecognized intrinsic error: {a:?}"),
            };
            p.token(Token::Intrinsic)?;
            let open = p.try_token(Token::OpenParen);
            let args = p.sequence_zero_or_more(Token::Comma)?;
            let args = args.into_boxed_slice();

            if open.is_some() {
                p.token(Token::CloseParen)?;
            }
            Some(Self { intrinsic, args })
        })
    }
}

impl Parse for Module {
    fn parse(p: &mut Parser) -> Option<Self> {
        let module_name = p.advance();
        match module_name.item() {
            Token::NewFile(name) => {
                let name = p.source_map().get(*name).0;
                let name = match file_name_to_module_name(name) {
                    Ok(o) => o,
                    Err(e) => {
                        p.push_error(p.span().with_item(e));
                        return None;
                    },
                };
                // intern all identifiers in the name
                let identifiers = name
                    .into_iter()
                    .map(|id| Identifier {
                        id:   p.intern(id),
                        span: p.span(),
                    })
                    .collect::<Vec<_>>()
                    .into_boxed_slice();
                let name = Path { identifiers };

                let nodes: Vec<_> = p.many::<SpannedItem<AstNode>>();

                Some(Module { name, nodes })
            },
            _ => None,
        }
    }
}

/// given a file path, construct a module name.
/// In an OS-independent way, finds the last `/src` in the path and uses the path after that.
/// Removes slashes, converts hyphens to underscores, and removes the `.petr` extension.
/// Returns a parse error if the name is not a valid identifier after these transformations.
/// A name is not a valid identifier if it contains spaces, starts with a number, or contains any symbols.
fn file_name_to_module_name(name: &str) -> Result<Vec<Rc<str>>, ParseErrorKind> {
    use std::path::Path;
    let path = Path::new(name);
    let name = path
        .components()
        .rev()
        .take_while(|comp| comp.as_os_str() != "src")
        .collect::<Vec<_>>()
        .into_iter()
        .rev()
        .map(|comp| comp.as_os_str().to_string_lossy().replace('-', "_").replace(".pt", ""))
        .map(Rc::from)
        .collect::<Vec<_>>();
    if let Some(part) = name.iter().find(|part| !is_valid_identifier(part)) {
        return Err(ParseErrorKind::InvalidIdentifier(part.to_string()));
    }
    Ok(name)
}

fn is_valid_identifier(name: &str) -> bool {
    let mut chars = name.chars();
    if let Some(first) = chars.next() {
        if !first.is_alphabetic() && first != '_' {
            return false;
        }
    }
    chars.all(|c| c.is_alphanumeric() || c == '_')
}
#[test]
fn test_file_name_to_module_name_simple() {
    let file_name = "src/main.pt";
    let expected = vec!["main".to_string()];
    let result = file_name_to_module_name(file_name)
        .unwrap()
        .into_iter()
        .map(|s| s.to_string())
        .collect::<Vec<_>>();
    assert_eq!(result, expected);
}

#[test]
fn test_file_name_to_module_name_with_hyphen() {
    let file_name = "src/my-module.pt";
    let expected = vec!["my_module".to_string()];
    let result = file_name_to_module_name(file_name)
        .unwrap()
        .into_iter()
        .map(|s| s.to_string())
        .collect::<Vec<_>>();
    assert_eq!(result, expected);
}

#[test]
fn test_file_name_to_module_name_nested_directory() {
    let file_name = "src/subdir/mysubmodule.pt";
    let expected = vec!["subdir".to_string(), "mysubmodule".to_string()];
    let result = file_name_to_module_name(file_name)
        .unwrap()
        .into_iter()
        .map(|s| s.to_string())
        .collect::<Vec<_>>();
    assert_eq!(result, expected);
}

#[test]
fn test_file_name_to_module_name_invalid_identifier() {
    let file_name = "src/123invalid.pt";
    let result = file_name_to_module_name(file_name);
    assert!(result.is_err());
}
