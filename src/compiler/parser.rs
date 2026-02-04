use crate::compiler::{ast, tokenizer};

struct Parser {
    tokens: Vec<tokenizer::Token>,
    pos: usize,
}

impl Parser {
    fn consume(&mut self, expected: Option<&str>) -> Result<&tokenizer::Token, String> {
        let token = &self.tokens[self.pos];

        if expected.is_some_and(|value| token.text != value) {
            return Err(format!(
                "{:?}: expected '{:?}'",
                token.loc,
                expected.unwrap()
            ));
        }

        self.pos += 1;
        if self.pos >= self.tokens.len() {
            self.pos = self.tokens.len() - 1;
        }

        Ok(token)
    }

    fn parse_int_literal(&self) -> ast::Expression {
        todo!()
    }

    fn parse_identifier(&self) -> ast::Expression {
        todo!()
    }

    fn parse_term(&self) -> ast::Expression {
        todo!()
    }

    fn parse_expression(&self) -> Result<ast::Expression, String> {
        todo!()
    }
}

pub fn parse(tokens: Vec<tokenizer::Token>) -> Result<ast::Expression, String> {
    let parser = Parser { tokens, pos: 0 };
    parser.parse_expression()
}
