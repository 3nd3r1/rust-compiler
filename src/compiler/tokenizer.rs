use regex::Regex;

#[derive(Debug, Clone, PartialEq)]
enum TokenKind {
    Identifier,
    IntLiteral,
    Other,
}

#[derive(Debug, Clone, PartialEq)]
struct TokenLocation {
    line: u32,
    column: u32,
}

#[derive(Debug, Clone, PartialEq)]
struct Token {
    kind: TokenKind,
    text: String,
    loc: TokenLocation,
}

pub fn tokenize(source_code: &str) -> Result<Vec<Token>, String> {
    let identifier = Regex::new(r"^[a-zA-Z_][a-zA-Z0-9_]*").unwrap();
    let literal = Regex::new(r"^[0-9]+").unwrap();
    let whitespace = Regex::new(r"^\s+").unwrap();

    let patterns = vec![
        (TokenKind::Identifier, identifier),
        (TokenKind::IntLiteral, literal),
    ];

    let mut remaining_code = source_code;
    let mut tokens = Vec::new();

    while !remaining_code.is_empty() {
        if let Some(matched) = whitespace.find(remaining_code) {
            remaining_code = &remaining_code[matched.end()..];
            continue;
        }

        let mut match_found = false;
        for (token_kind, pattern) in &patterns {
            if let Some(matched) = pattern.find(remaining_code) {
                let token = Token {
                    kind: token_kind.clone(),
                    text: matched.as_str().to_string(),
                    loc: TokenLocation { line: 0, column: 0 },
                };
                tokens.push(token);
                remaining_code = &remaining_code[matched.end()..];
                match_found = true;
                break;
            }
        }

        if !match_found {
            return Err("No match found".to_string());
        }
    }

    Ok(tokens)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_tokenizer_basics() {
        assert_eq!(tokenize("if  3\nwhile").unwrap(), vec!["if", "3", "while"]);
    }

    #[test]
    fn test_tokenizer_valid_indentifiers() {
        assert_eq!(tokenize("hello").unwrap(), vec!["hello"]);
        assert_eq!(tokenize("HELLO").unwrap(), vec!["HELLO"]);
        assert_eq!(tokenize("hEl_LO").unwrap(), vec!["hEl_LO"]);
        assert_eq!(tokenize("__hello").unwrap(), vec!["__hello"]);
        assert_eq!(tokenize("_hEl_LO").unwrap(), vec!["_hEl_LO"]);
        assert_eq!(tokenize("_hE12lLO").unwrap(), vec!["_hE12lLO"]);
    }

    #[test]
    fn test_tokenizer_literals() {
        assert_eq!(tokenize("0").unwrap(), vec!["0"]);
        assert_eq!(tokenize("123").unwrap(), vec!["123"]);
        assert_eq!(tokenize("123 123  11").unwrap(), vec!["123", "123", "11"]);
        assert_eq!(
            tokenize("000 123 123  11").unwrap(),
            vec!["000", "123", "123", "11"]
        );
    }

    #[test]
    fn test_tokenizer_whitespace() {
        assert_eq!(
            tokenize("moi  miten\n menee\t sulla").unwrap(),
            vec!["moi", "miten", "menee", "sulla"]
        );
    }

    #[test]
    fn test_tokenizer_invalid_characters() {
        assert!(tokenize("@").is_err());
        assert!(tokenize("pete@gmail.fi").is_err());
        assert!(tokenize("^hi$").is_err());
        assert!(tokenize("hi$").is_err());
    }

    #[test]
    fn test_tokenizer_mixed() {
        assert_eq!(
            tokenize("moi1 123  hello 23 21 _var1\n if 23").unwrap(),
            vec!["moi1", "123", "hello", "23", "21", "_var1", "if", "23"]
        );
    }
}
