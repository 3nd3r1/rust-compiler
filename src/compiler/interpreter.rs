use crate::compiler::ast;

#[derive(Debug, Clone, PartialEq)]
enum Value {
    Int(i32),
    Bool(bool),
    None,
}

fn interpret(node: ast::Expression) -> Result<Value, String> {
    match node.kind {
        ast::ExpressionKind::IntLiteral { value } => Ok(Value::Int(value)),
        _ => Err(format!("unexpected expression {:?}", node.kind)),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::compiler::{common, parser, tokenizer};

    fn loc() -> common::Location {
        common::Location { line: 0, column: 0 }
    }

    fn interpret_without_loc(source_code: &str) -> Result<Value, String> {
        let mut tokens = tokenizer::tokenize(source_code).unwrap();
        tokens = tokens
            .into_iter()
            .map(|t| tokenizer::Token {
                kind: t.kind,
                text: t.text,
                loc: loc(),
            })
            .collect();

        interpret(parser::parse(tokens).unwrap())
    }

    #[test]
    fn test_interpreter_basics() {
        assert_eq!(interpret_without_loc("2 + 3").unwrap(), Value::Int(5));
    }
}
