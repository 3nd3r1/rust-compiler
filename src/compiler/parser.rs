use crate::compiler::{ast, tokenizer};

pub fn parse(tokens: Vec<tokenizer::Token>) -> ast::Expression {
    let mut pos = 0 as usize;

    return ast::Expression::Literal {};
}

fn peek(tokens: &Vec<tokenizer::Token>, pos: usize) -> &tokenizer::Token {
    &tokens[pos]
}

fn consume(tokens: &Vec<tokenizer::Token>, pos: usize) -> &tokenizer::Token {
    let token = peek(tokens, pos);
    token
}

fn parse_int_literal(tokens: &Vec<tokenizer::Token>, pos: usize) -> ast::Expression {
    ast::Expression::Literal {}
}

fn parse_identifier(tokens: &Vec<tokenizer::Token>, pos: usize) -> ast::Expression {
    ast::Expression::Identifier {}
}

fn parse_term(tokens: &Vec<tokenizer::Token>, pos: usize) -> ast::Expression {
    if peek(tokens, pos).kind == tokenizer::TokenKind::IntLiteral {
        return parse_int_literal(tokens, pos);
    } else {
        return parse_identifier(tokens, pos);
    }
}

fn parse_expression(tokens: &Vec<tokenizer::Token>, pos: usize) -> ast::Expression {
    parse_term(tokens, pos)
}
