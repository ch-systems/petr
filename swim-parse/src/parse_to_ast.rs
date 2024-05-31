use std::rc::Rc;

// using this crate's Parser, parse an AST.
use swim_ast::*;
use swim_utils::{Identifier, SpannedItem};

use crate::{
    parser::{Parse, ParseErrorKind, Token},
    Parser,
};
impl Parse for TypeDeclaration {
    fn parse(p: &mut Parser) -> Option<Self> {
        p.with_help(
            "encountered while parsing type declaration",
            |p| -> Option<Self> {
                p.token(Token::TypeKeyword)?;
                let name = p.parse()?;
                if *p.peek().item() != Token::Equals {
                    // if there's no equals, then this is a type with no variants
                    return Some(Self {
                        name,
                        variants: vec![].into_boxed_slice(),
                    });
                }
                p.token(Token::Equals)?;
                let variants = p.sequence::<TypeVariant>(Token::Pipe)?;
                Some(Self {
                    name,
                    variants: variants.into_boxed_slice(),
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
            Token::FunctionKeyword => Some(AstNode::FunctionDeclaration(p.parse()?)),
            Token::TypeKeyword => Some(AstNode::TypeDeclaration(p.parse()?)),
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
                p.token(Token::FunctionKeyword)?;
                let name = p.parse()?;
                p.token(Token::OpenParen)?;
                let parameters = if *p.peek().item() == Token::CloseParen {
                    vec![].into_boxed_slice()
                } else {
                    p.sequence(Token::Comma)?.into_boxed_slice()
                };
                p.token(Token::CloseParen)?;
                p.token(Token::ReturnsKeyword)?;
                let return_type = p.parse()?;
                let body = p.parse()?;
                Some(Self {
                    name,
                    parameters,
                    return_type,
                    body,
                })
            },
        )
    }
}
impl Parse for VariableExpression {
    fn parse(p: &mut Parser) -> Option<Self> {
        let id = Identifier::parse(p)?;
        Some(VariableExpression { name: id })
    }
}
impl Parse for Literal {
    fn parse(p: &mut Parser) -> Option<Self> {
        let tok = p.advance();
        match tok.item() {
            Token::Integer => Some(Literal::Integer(
                p.slice().parse().expect("lexer should have verified this"),
            )),
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
            let ident = Identifier::parse(p)?;
            Some(Self { ty_name: ident })
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
            Token::Integer => Some(Expression::Literal(p.parse()?)),
            // TODO might not want to do variables this way
            // may have to advance and peek to see if its a fn call etc
            Token::Identifier => Some(Expression::Variable(p.parse()?)),
            a => todo!("need to parse expr {a:?}"),
        }
    }
}
