use std::rc::Rc;

// using this crate's Parser, parse an AST.
use swim_ast::*;
use swim_utils::{Identifier, SpannedItem};

use crate::{
    parser::{Parse, ParseErrorKind, Token},
    Parser,
};

impl Parse for FunctionCall {
    fn parse(p: &mut Parser) -> Option<Self> {
        p.with_help(
            "encountered while parsing function call",
            |p| -> Option<Self> {
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
            },
        )
    }
}

impl Parse for TypeDeclaration {
    fn parse(p: &mut Parser) -> Option<Self> {
        p.with_help(
            "encountered while parsing type declaration",
            |p| -> Option<Self> {
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
            },
        )
    }
}

impl Parse for TypeVariant {
    fn parse(p: &mut Parser) -> Option<Self> {
        p.with_help(
            "encountered while parsing type variant",
            |p| -> Option<Self> {
                let mut buf = vec![];
                let name = p.parse()?;
                loop {
                    let peek = *p.peek().item();
                    if peek == Token::TyMarker {
                        let field: Ty = p.parse()?;
                        buf.push(field);
                    } else {
                        break;
                    }
                }
                Some(Self {
                    name,
                    fields: buf.into_boxed_slice(),
                })
            },
        )
    }
}

impl Parse for AstNode {
    fn parse(p: &mut Parser) -> Option<Self> {
        match p.peek().item() {
            Token::FunctionKeyword | Token::ExportFunctionKeyword => {
                Some(AstNode::FunctionDeclaration(p.parse()?))
            }
            Token::TypeKeyword | Token::ExportTypeKeyword => {
                Some(AstNode::TypeDeclaration(p.parse()?))
            }
            Token::Eof => None,
            a => {
                let span = p.peek().span();
                p.push_error(span.with_item(ParseErrorKind::ExpectedOneOf(
                    vec![Token::FunctionKeyword, Token::TypeKeyword, Token::Eof],
                    *a,
                )));
                None
            }
        }
    }
}
impl Parse for FunctionDeclaration {
    fn parse(p: &mut Parser) -> Option<Self> {
        p.with_help(
            "encountered while parsing function declaration",
            |p| -> Option<Self> {
                let tok = p.one_of([Token::FunctionKeyword, Token::ExportFunctionKeyword])?;
                let visibility = match tok.item() {
                    Token::FunctionKeyword => Visibility::Local,
                    Token::ExportFunctionKeyword => Visibility::Exported,
                    _ => unreachable!(),
                };
                let name = p.parse()?;
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
            },
        )
    }
}
impl Parse for Literal {
    fn parse(p: &mut Parser) -> Option<Self> {
        let tok = p.advance();
        match tok.item() {
            Token::Integer => Some(Literal::Integer(
                p.slice().parse().expect("lexer should have verified this"),
            )),
            Token::String => Some(Literal::String(Rc::from(
                &p.slice()[1..p.slice().len() - 1],
            ))),
            Token::True => Some(Literal::Boolean(true)),
            Token::False => Some(Literal::Boolean(false)),
            _ => {
                p.push_error(
                    p.span()
                        .with_item(ParseErrorKind::ExpectedToken(Token::Integer, *tok.item())),
                );
                None
            }
        }
    }
}
impl Parse for FunctionParameter {
    fn parse(p: &mut Parser) -> Option<Self> {
        p.with_help(
            "encountered while parsing function parameter",
            |p| -> Option<Self> {
                let name: Identifier = p.parse()?;
                p.one_of([Token::InKeyword, Token::IsInSymbol])?;
                let ty: Ty = p.parse()?;
                Some(FunctionParameter { name, ty })
            },
        )
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
            }
        }
    }
}
impl Parse for Identifier {
    fn parse(p: &mut Parser) -> Option<Self> {
        let identifier = p.advance();
        if *identifier.item() != Token::Identifier {
            p.push_error(
                p.span()
                    .with_item(ParseErrorKind::ExpectedIdentifier(p.slice().to_string())),
            );
        }
        let slice = Rc::from(p.slice());
        let id = p.intern(slice);
        Some(Identifier { id })
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
        match p.peek().item() {
            item if item.is_operator() => {
                let op: SpannedItem<Operator> = p.parse()?;
                // parse prefix notation operator expression
                let lhs: SpannedItem<Expression> = p.parse()?;
                let rhs: SpannedItem<Expression> = p.parse()?;
                Some(Expression::Operator(Box::new(OperatorExpression {
                    lhs,
                    rhs,
                    op,
                })))
            }
            // TODO might not want to do variables this way
            // may have to advance and peek to see if its a fn call etc
            Token::Identifier => Some(Expression::Variable(p.parse()?)),
            Token::OpenBracket => Some(Expression::List(p.parse()?)),
            Token::Tilde => Some(Expression::FunctionCall(p.parse()?)),
            Token::True | Token::False | Token::String | Token::Integer => {
                Some(Expression::Literal(p.parse()?))
            }
            Token::Intrinsic => Some(Expression::IntrinsicCall(p.parse()?)),
            a => {
                println!("need to parse expr {a:?} {}", p.slice());
                None
            }
        }
    }
}

impl Parse for IntrinsicCall {
    fn parse(p: &mut Parser) -> Option<Self> {
        p.with_help(
            "encountered while parsing intrinsic call",
            |p| -> Option<Self> {
                let name = p.slice().to_string();
                let intrinsic = match &name[1..] {
                    "puts" => Intrinsic::Puts,
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
            },
        )
    }
}
