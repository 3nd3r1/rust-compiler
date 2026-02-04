use crate::compiler::{ast, tokenizer};

struct Parser {
    tokens: Vec<tokenizer::Token>,
    pos: usize,
}

impl Parser {
    fn peek(&self) -> &tokenizer::Token {
        self.tokens.get(self.pos).unwrap()
    }

    fn consume(
        &mut self,
        token_kind: tokenizer::TokenKind,
        expected: Option<&str>,
    ) -> Result<&tokenizer::Token, String> {
        let token = &self.tokens[self.pos];

        if token.kind != token_kind {
            return Err(format!("{:?}: expected an {:?}", token.loc, token_kind));
        }

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

    fn parse_int_literal(&mut self) -> Result<ast::Expression, String> {
        let token = self.consume(tokenizer::TokenKind::IntLiteral, None)?;
        Ok(ast::Expression::Literal {
            value: token
                .text
                .parse::<i32>()
                .map_err(|_| format!("{:?}: invalid number", token.loc))?,
        })
    }

    fn parse_identifier(&mut self) -> Result<ast::Expression, String> {
        let token = self.consume(tokenizer::TokenKind::Identifier, None)?;
        Ok(ast::Expression::Identifier {
            value: token.text.clone(),
        })
    }

    fn parse_term(&mut self) -> Result<ast::Expression, String> {
        match self.peek().kind {
            tokenizer::TokenKind::IntLiteral => return self.parse_int_literal(),
            tokenizer::TokenKind::Identifier => return self.parse_identifier(),
            _ => {
                return Err(format!(
                    "{:?}: expected an integer or identifier",
                    self.peek().loc
                ));
            }
        }
    }

    fn parse_expression(&self) -> Result<ast::Expression, String> {
        todo!()
    }
}

pub fn parse(tokens: Vec<tokenizer::Token>) -> Result<ast::Expression, String> {
    let parser = Parser { tokens, pos: 0 };
    parser.parse_expression()
}
