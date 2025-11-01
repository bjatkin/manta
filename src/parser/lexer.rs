use std::fmt;
use std::collections::HashSet;

/// Byte span in the source (start..end)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
	pub start: usize,
	pub end: usize,
}

impl Span {
	pub fn new(start: usize, end: usize) -> Self {
		Span { start, end }
	}
}

/// The kind of Token produced by the lexer.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenKind {
	Ident,
	Int,
	Float,
	Str,
	Keyword,
	Operator,
	Punct,
	Eof,
}

/// A token produced by the lexer. `lexeme` contains raw text for ids/numbers/strings
/// (strings are returned unescaped), or None for simple punctuation/EOF.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
	pub kind: TokenKind,
	pub lexeme: Option<String>,
	pub span: Span,
}

impl Token {
	pub fn new(kind: TokenKind, lexeme: Option<String>, span: Span) -> Self {
		Token { kind, lexeme, span }
	}

	pub fn eof(span: Span) -> Self {
		Token { kind: TokenKind::Eof, lexeme: None, span }
	}
}

#[derive(Debug)]
pub struct LexError {
	pub message: String,
	pub span: Span,
}

impl fmt::Display for LexError {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "LexError at {}..{}: {}", self.span.start, self.span.end, self.message)
	}
}

impl std::error::Error for LexError {}

/// Minimal lexer. Uses a byte cursor but iterates by `char`s for UTF-8 correctness.
#[derive(Clone)]
pub struct Lexer {
	input: String,
	pos: usize,
	len: usize,
}

impl Lexer {
	/// Create a new lexer from source text.
	pub fn new(source: &str) -> Self {
		Lexer {
			input: source.to_string(),
			pos: 0,
			len: source.len(),
		}
	}

	/// Non-consuming peek of the next token kind (may return LexError if scanning fails).
	pub fn peek(&self) -> Result<TokenKind, LexError> {
		let mut cloned = self.clone();
		let tok = cloned.next_token()?;
		Ok(tok.kind)
	}

	/// Return the next token.
	pub fn next_token(&mut self) -> Result<Token, LexError> {
		self.skip_whitespace_and_comments()?;

		if self.eof() {
			let span = Span::new(self.pos, self.pos);
			return Ok(Token::eof(span));
		}

		// determine by first char
		let ch = self.current_char().unwrap();

		if is_ident_start(ch) {
			return Ok(self.read_ident_or_keyword());
		}

		if ch.is_ascii_digit() {
			return Ok(self.read_number());
		}

		if ch == '"' {
			return self.read_string();
		}

		// operators and punctuation
		return Ok(self.read_operator_or_punct());
	}

	// internal helpers for the lexer

	fn eof(&self) -> bool {
		self.pos >= self.len
	}

	fn current_char(&self) -> Option<char> {
		self.input[self.pos..].chars().next()
	}

	fn peek_char_n(&self, n: usize) -> Option<char> {
		let mut it = self.input[self.pos..].chars();
		(0..n).for_each(|_| { let _ = it.next(); });
		it.next()
	}

	fn bump(&mut self) -> Option<char> {
		if let Some(ch) = self.current_char() {
			let adv = ch.len_utf8();
			self.pos += adv;
			Some(ch)
		} else {
			None
		}
	}

	fn eat_while<F>(&mut self, mut cond: F) -> String
	where
		F: FnMut(char) -> bool,
	{
		let start = self.pos;
		while let Some(ch) = self.current_char() {
			if cond(ch) {
				self.bump();
			} else {
				break;
			}
		}
		self.input[start..self.pos].to_string()
	}

	fn skip_whitespace_and_comments(&mut self) -> Result<(), LexError> {
		loop {
			// skip whitespace
			let mut progressed = false;
			while let Some(ch) = self.current_char() {
				if ch.is_whitespace() {
					progressed = true;
					self.bump();
				} else {
					break;
				}
			}

			// comments
			if self.input[self.pos..].starts_with("//") {
				progressed = true;
				// consume until newline or EOF
				self.pos += 2;
				while let Some(ch) = self.current_char() {
					self.bump();
					if ch == '\n' { break; }
				}
			} else if self.input[self.pos..].starts_with("/*") {
				progressed = true;
				// consume block comment until */
				self.pos += 2;
				loop {
					if self.eof() {
						return Err(LexError { message: "Unterminated block comment".to_string(), span: Span::new(self.pos, self.pos) });
					}
					if self.input[self.pos..].starts_with("*/") {
						self.pos += 2;
						break;
					}
					self.bump();
				}
			}

			if !progressed { break; }
		}
		Ok(())
	}

	fn read_ident_or_keyword(&mut self) -> Token {
		let start = self.pos;
		let lex = self.eat_while(|ch| is_ident_continue(ch));
		let end = self.pos;

		let keywords: HashSet<&'static str> = [
			"let", "fn", "if", "else", "while", "for", "return", "true", "false",
			"match", "enum", "struct", "impl", "const", "pub", "mod",
		]
		.iter()
		.cloned()
		.collect();

		let kind = if keywords.contains(lex.as_str()) {
			TokenKind::Keyword
		} else {
			TokenKind::Ident
		};

		Token::new(kind, Some(lex), Span::new(start, end))
	}

	fn read_number(&mut self) -> Token {
		let start = self.pos;
		let mut seen_dot = false;
		let mut seen_exp = false;

		// integer/fraction part
		let mut s = String::new();
		while let Some(ch) = self.current_char() {
			if ch == '_' {
				// allow underscores between digits
				self.bump();
				s.push('_');
				continue;
			}
			if ch.is_ascii_digit() {
				self.bump();
				s.push(ch);
				continue;
			}
			if ch == '.' && !seen_dot {
				// check next char to avoid treating '.' in '..' as float
				if let Some(nc) = self.peek_char_n(1) {
					if nc.is_ascii_digit() {
						seen_dot = true;
						self.bump();
						s.push('.');
						continue;
					}
				}
			}
			break;
		}

		// exponent part
		if let Some(ch) = self.current_char() {
			if ch == 'e' || ch == 'E' {
				seen_exp = true;
				self.bump();
				s.push('e');
				if let Some(sign) = self.current_char() {
					if sign == '+' || sign == '-' {
						self.bump();
						s.push(sign);
					}
				}
				// digits of exponent
				let digits = self.eat_while(|c| c.is_ascii_digit() || c == '_');
				s.push_str(&digits);
			}
		}

		let end = self.pos;
		let kind = if seen_dot || seen_exp { TokenKind::Float } else { TokenKind::Int };
		Token::new(kind, Some(s), Span::new(start, end))
	}

	fn read_string(&mut self) -> Result<Token, LexError> {
		let start = self.pos;
		// consume the opening quote char
		let quote = self.bump().unwrap();
		debug_assert!(quote == '"');

		let mut out = String::new();
		while let Some(ch) = self.current_char() {
			self.bump();
			if ch == '"' {
				let end = self.pos;
				return Ok(Token::new(TokenKind::Str, Some(out), Span::new(start, end)));
			}
			if ch == '\\' {
				if let Some(esc) = self.current_char() {
					self.bump();
					match esc {
						'n' => out.push('\n'),
						't' => out.push('\t'),
						'r' => out.push('\r'),
						'\\' => out.push('\\'),
						'"' => out.push('"'),
						other => {
							// Unknown escape; keep it verbatim
							out.push(other);
						}
					}
					continue;
				} else {
					return Err(LexError { message: "Unfinished escape in string".to_string(), span: Span::new(self.pos, self.pos) });
				}
			}
			out.push(ch);
		}

		Err(LexError { message: "Unterminated string literal".to_string(), span: Span::new(start, self.pos) })
	}

	fn read_operator_or_punct(&mut self) -> Token {
		let start = self.pos;
		let ch = self.bump().unwrap();
		let next = self.current_char();

		// multi-char operators
		if let Some(nc) = next {
			let two = format!("{}{}", ch, nc);
			let two_ops: [&str; 10] = ["==", "!=", "<=", ">=", "&&", "||", ":=", "+=", "-=", "**"];
			if two_ops.contains(&two.as_str()) {
				self.bump();
				return Token::new(TokenKind::Operator, Some(two), Span::new(start, self.pos));
			}
		}

		// single char punctuation vs operator
		let puncts: [char; 10] = ['(', ')', '{', '}', '[', ']', ',', ';', ':', '.'];
		if puncts.contains(&ch) {
			return Token::new(TokenKind::Punct, Some(ch.to_string()), Span::new(start, self.pos));
		}

		// otherwise treat as operator
		Token::new(TokenKind::Operator, Some(ch.to_string()), Span::new(start, self.pos))
	}
}

/// Iterator adapter that yields tokens including EOF, then terminates.
pub struct LexerIntoIter {
	lexer: Lexer,
	done: bool,
}

impl IntoIterator for Lexer {
	type Item = Result<Token, LexError>;
	type IntoIter = LexerIntoIter;

	fn into_iter(self) -> Self::IntoIter {
		LexerIntoIter { lexer: self, done: false }
	}
}

impl Iterator for LexerIntoIter {
	type Item = Result<Token, LexError>;

	fn next(&mut self) -> Option<Self::Item> {
		if self.done {
			return None;
		}

		match self.lexer.next_token() {
			Ok(tok) => {
				if tok.kind == TokenKind::Eof {
					self.done = true;
					Some(Ok(tok))
				} else {
					Some(Ok(tok))
				}
			}
			Err(e) => {
				self.done = true;
				Some(Err(e))
			}
		}
	}
}

fn is_ident_start(ch: char) -> bool {
	ch == '_' || ch.is_alphabetic()
}

fn is_ident_continue(ch: char) -> bool {
	ch == '_' || ch.is_alphanumeric()
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn lex_simple_declaration() {
		let src = "let x = 42;";
		let iter: Vec<_> = Lexer::new(src).into_iter().map(|r| r.unwrap()).collect();
		assert_eq!(iter.len(), 6);
		assert_eq!(iter[0].kind, TokenKind::Keyword);
		assert_eq!(iter[0].lexeme.as_deref(), Some("let"));
		assert_eq!(iter[1].kind, TokenKind::Ident);
		assert_eq!(iter[1].lexeme.as_deref(), Some("x"));
		assert_eq!(iter[2].kind, TokenKind::Operator);
		assert_eq!(iter[2].lexeme.as_deref(), Some("="));
		assert_eq!(iter[3].kind, TokenKind::Int);
		assert_eq!(iter[3].lexeme.as_deref(), Some("42"));
		assert_eq!(iter[4].kind, TokenKind::Punct);
		assert_eq!(iter[4].lexeme.as_deref(), Some(";"));
		assert_eq!(iter[5].kind, TokenKind::Eof);
	}

	#[test]
	fn lex_string_and_escape() {
		let src = "\"hello\\n\"";
		let toks: Vec<_> = Lexer::new(src).into_iter().map(|r| r.unwrap()).collect();
		assert_eq!(toks.len(), 2);
		assert_eq!(toks[0].kind, TokenKind::Str);
		assert_eq!(toks[0].lexeme.as_deref(), Some("hello\n"));
	}

	#[test]
	fn lex_comments_and_whitespace() {
		let src = "  // top comment\nlet/*block*/ x = 1; // end\n";
		let toks: Vec<_> = Lexer::new(src).into_iter().map(|r| r.unwrap()).collect();
		// tokens: let, x, =, 1, ;, eof
		assert_eq!(toks.len(), 6);
		assert_eq!(toks[0].kind, TokenKind::Keyword);
		assert_eq!(toks[0].lexeme.as_deref(), Some("let"));
		assert_eq!(toks[1].kind, TokenKind::Ident);
		assert_eq!(toks[1].lexeme.as_deref(), Some("x"));
		assert_eq!(toks[2].kind, TokenKind::Operator);
		assert_eq!(toks[2].lexeme.as_deref(), Some("="));
		assert_eq!(toks[3].kind, TokenKind::Int);
		assert_eq!(toks[3].lexeme.as_deref(), Some("1"));
		assert_eq!(toks[4].kind, TokenKind::Punct);
		assert_eq!(toks[4].lexeme.as_deref(), Some(";"));
		assert_eq!(toks[5].kind, TokenKind::Eof);
	}

	#[test]
	fn lex_numbers_and_floats() {
		let src = "3.14 1e10 2.5e-3 1_000";
		let toks: Vec<_> = Lexer::new(src).into_iter().map(|r| r.unwrap()).collect();
		// 3 floats/ints + eof
		assert_eq!(toks.len(), 5);
		assert_eq!(toks[0].kind, TokenKind::Float);
		assert_eq!(toks[0].lexeme.as_deref(), Some("3.14"));
		assert_eq!(toks[1].kind, TokenKind::Float);
		assert_eq!(toks[1].lexeme.as_deref(), Some("1e10"));
		assert_eq!(toks[2].kind, TokenKind::Float);
		assert_eq!(toks[2].lexeme.as_deref(), Some("2.5e-3"));
		assert_eq!(toks[3].kind, TokenKind::Int);
		assert_eq!(toks[3].lexeme.as_deref(), Some("1_000"));
	}

	#[test]
	fn lex_multi_char_operators() {
		let src = "a == b && c != d <= e >= f := += **";
		let toks: Vec<_> = Lexer::new(src).into_iter().map(|r| r.unwrap()).collect();
		// expected operator lexemes in order (after identifiers)
		let expected_ops = vec!["==", "&&", "!=", "<=", ">=", ":=", "+=", "**"];
		// filter operator tokens from the stream
		let ops: Vec<String> = toks.into_iter()
			.filter(|t| t.kind == TokenKind::Operator)
			.filter_map(|t| t.lexeme.clone())
			.collect();
		assert_eq!(ops, expected_ops);
	}

	#[test]
	fn lex_variant_and_assignment() {
		let src = ".Ok x = 1;";
		let toks: Vec<_> = Lexer::new(src).into_iter().map(|r| r.unwrap()).collect();
		// .Ok should be a punct (.) then Ident "Ok"
		assert_eq!(toks[0].kind, TokenKind::Punct);
		assert_eq!(toks[0].lexeme.as_deref(), Some("."));
		assert_eq!(toks[1].kind, TokenKind::Ident);
		assert_eq!(toks[1].lexeme.as_deref(), Some("Ok"));
		assert_eq!(toks[2].kind, TokenKind::Ident);
		assert_eq!(toks[2].lexeme.as_deref(), Some("x"));
		assert_eq!(toks[3].lexeme.as_deref(), Some("="));
		assert_eq!(toks[4].kind, TokenKind::Int);
		assert_eq!(toks[4].lexeme.as_deref(), Some("1"));
	}

	#[test]
	fn unterminated_string_error() {
		let mut lx = Lexer::new("\"no end");
		match lx.next_token() {
			Err(e) => assert!(e.message.contains("Unterminated string")),
			Ok(_) => panic!("expected lex error for unterminated string"),
		}
	}

	#[test]
	fn lex_type_enum_tokens() {
		let src = "type ErrWrite enum { Ok IOError }";
		let toks: Vec<_> = Lexer::new(src).into_iter().map(|r| r.unwrap()).collect();
		// type (ident), ErrWrite (ident), enum (keyword), {, Ok, IOError, }, eof
		assert_eq!(toks[0].lexeme.as_deref(), Some("type"));
		assert_eq!(toks[1].lexeme.as_deref(), Some("ErrWrite"));
		assert_eq!(toks[2].kind, TokenKind::Keyword);
		assert_eq!(toks[3].lexeme.as_deref(), Some("{"));
		assert_eq!(toks[4].lexeme.as_deref(), Some("Ok"));
		assert_eq!(toks[5].lexeme.as_deref(), Some("IOError"));
	}

	#[test]
	fn lex_fn_signature_tokens() {
		let src = "fn write_and_cleanup(path string) ErrWrite { }";
		let toks: Vec<_> = Lexer::new(src).into_iter().map(|r| r.unwrap()).collect();
		assert_eq!(toks[0].kind, TokenKind::Keyword);
		assert_eq!(toks[0].lexeme.as_deref(), Some("fn"));
		assert_eq!(toks[1].lexeme.as_deref(), Some("write_and_cleanup"));
		assert_eq!(toks[2].lexeme.as_deref(), Some("("));
		assert_eq!(toks[3].lexeme.as_deref(), Some("path"));
		assert_eq!(toks[4].lexeme.as_deref(), Some("string"));
		assert_eq!(toks[5].lexeme.as_deref(), Some(")"));
		assert_eq!(toks[6].lexeme.as_deref(), Some("ErrWrite"));
		assert_eq!(toks[7].lexeme.as_deref(), Some("{"));
		assert_eq!(toks[8].lexeme.as_deref(), Some("}"));
	}

	#[test]
	fn lex_try_catch_line_tokens() {
		let src = ".Ok(f) := try open(path) catch { return .IOError }";
		let toks: Vec<_> = Lexer::new(src).into_iter().map(|r| r.unwrap()).collect();
		// . Ok ( f ) := try open ( path ) catch { return . IOError }
		let seq: Vec<_> = toks.iter().map(|t| t.lexeme.as_deref()).collect();
		assert_eq!(seq[0], Some("."));
		assert_eq!(seq[1], Some("Ok"));
		assert_eq!(seq[2], Some("("));
		assert_eq!(seq[3], Some("f"));
		assert_eq!(seq[4], Some(")"));
		assert_eq!(seq[5], Some(":="));
		assert_eq!(seq[6], Some("try"));
		assert_eq!(seq[7], Some("open"));
		assert_eq!(seq[8], Some("("));
		assert_eq!(seq[9], Some("path"));
		assert_eq!(seq[10], Some(")"));
		assert_eq!(seq[11], Some("catch"));
		assert_eq!(seq[12], Some("{"));
		assert_eq!(seq[13], Some("return"));
		assert_eq!(seq[14], Some("."));
		assert_eq!(seq[15], Some("IOError"));
		assert_eq!(seq[16], Some("}"));
	}

	#[test]
	fn lex_pointer_and_nil_tokens() {
		let src = "p := maybe_alloc(false); if p != nil { print(*p); free(p); }";
		let toks: Vec<_> = Lexer::new(src).into_iter().map(|r| r.unwrap()).collect();
		// verify full token lexeme sequence
		let seq: Vec<_> = toks.iter().map(|t| t.lexeme.as_deref()).collect();
		let expected = vec![
			Some("p"), Some(":="), Some("maybe_alloc"), Some("("), Some("false"), Some(")"), Some(";"),
			Some("if"), Some("p"), Some("!="), Some("nil"), Some("{"), Some("print"), Some("("), Some("*"), Some("p"), Some(")"), Some(";"), Some("free"), Some("("), Some("p"), Some(")"), Some(";"), Some("}"), None
		];
		assert_eq!(seq, expected);
	}
}