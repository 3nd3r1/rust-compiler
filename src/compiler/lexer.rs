use regex::Regex;

pub fn tokenize(source_code: &str) -> Vec<String> {
    let identifier = Regex::new(r"^[a-zA-Z_][a-zA-Z0-9_]*").unwrap();
    let literal = Regex::new(r"^[0-9]+").unwrap();
    let whitespace = Regex::new(r"^\ +").unwrap();

    let patterns = vec![identifier, literal];

    let mut remaining_code = source_code;
    let mut tokens = Vec::new();

    while !remaining_code.is_empty() {
        if whitespace.is_match(remaining_code) {
            let matched = whitespace.find(remaining_code).unwrap();
            remaining_code = &remaining_code[matched.end()..];
            continue;
        }

        for pattern in &patterns {
            let matched = pattern.find(remaining_code).unwrap();
            let token = matched.as_str();

            tokens.push(token.to_string());
        }
    }

    tokens
}
