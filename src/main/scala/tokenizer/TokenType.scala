package minijava
package tokenizer

// NOTES:
// PAREN = ()
// BRACE = {}
// BRACKET = []

enum TokenType {
  case

  // Single Characters
  LEFT_PAREN,
  RIGHT_PAREN,
  LEFT_BRACE,
  RIGHT_BRACE,
  LEFT_BRACKET,
  RIGHT_BRACKET,
  PERIOD,
  EQUAL,
  SEMICOLON,
  COMMA,

  // Operators
  AND,
  LESS,
  PLUS,
  DASH,
  STAR,
  BANG,

  // Keywords
  CLASS,
  PUBLIC,
  STATIC,
  // VOID,
  EXTENDS,
  IF,
  ELSE,
  WHILE,
  TRUE,
  FALSE,
  NEW,
  RETURN,
  FOR,

  // Literals
  IDENTIFIER,
  STRING,
  INTEGER,

  // Special
  PRINT,
  EOF

}
