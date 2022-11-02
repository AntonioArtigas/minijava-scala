package minijava
package tokenizer

import scala.collection.mutable.ListBuffer

object Tokenizer {
  val KEYWORDS = Map(
    "class" -> TokenType.CLASS,
    "public" -> TokenType.PUBLIC,
    "static" -> TokenType.STATIC,
    // "void" -> TokenType.VOID, // NOTE: Change void to a identifier now
    "extends" -> TokenType.EXTENDS,
    "if" -> TokenType.IF,
    "else" -> TokenType.ELSE,
    "while" -> TokenType.WHILE,
    "true" -> TokenType.TRUE,
    "false" -> TokenType.FALSE,
    "new" -> TokenType.NEW,
    "return" -> TokenType.RETURN,
    "this" -> TokenType.THIS
  ).withDefaultValue(TokenType.IDENTIFIER) // if the string given is not in the map, it's an identifier
}

/** Simple handwritten tokenizer, nothing fancy.
  *
  * @param source
  *   Source to tokenize.
  */
class Tokenizer(private val source: String) {
  private var start = 0
  private var current = 0
  private var line = 1

  private val buffer = ListBuffer[Token]()

  private def isAtEnd: Boolean = current >= source.length

  /** Eat current token and move cursor forward one
    *
    * @return
    *   The consumed token
    */
  private def advance(): Char = {
    current += 1
    source.charAt(current - 1)
  }

  /** See current token without changing cursor position
    *
    * @return
    *   Curren token
    */
  private def peek(): Char = if (isAtEnd) {
    '\u0000'
  } else {
    source.charAt(current)
  }

  /** Check if next token matches expected, if so go advance and return true,
    * otherwise just return false
    *
    * @param expected
    *   Token to match against
    * @return
    *   If the token was matched to the next one
    */
  private def isMatch(expected: Char): Boolean = {
    if (isAtEnd) {
      return false
    }
    if (source.charAt(current) != expected) {
      return false
    }

    current += 1
    true
  }

  // ASCII utilities
  private def isDigit(c: Char): Boolean = c >= '0' && c <= '9'

  private def isAlpha(c: Char): Boolean =
    (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_'

  private def isAlphaNumeric(c: Char): Boolean = isDigit(c) || isAlpha(c)

  // Can handle multi-line strings, should probably error on that...
  private def string(): Unit = {
    // consume all the characters before the closing string
    while (peek() != '"' && !isAtEnd) {
      if (peek() != '\n') {
        line += 1
      }
      advance()
    }

    if (isAtEnd) {
      MiniJava.error(line, "Unterminated string.")
    }

    // closing "
    advance()

    val text = source.substring(start + 1, current - 1)
    buffer.addOne(Token(TokenType.STRING, text, line))
  }

  private def integer(): Unit = {
    // keep eating numbers
    while (isDigit(peek())) {
      advance()
    }

    val text = source.substring(start, current)
    buffer.addOne(Token(TokenType.INTEGER, text, line))
  }

  private def identifier(): Unit = {
    while (isAlphaNumeric(peek())) {
      advance()
    }

    val text = source.substring(start, current)
    val typ = Tokenizer.KEYWORDS(text)
    buffer.addOne(Token(typ, text, line))
  }

  private def scanToken(): Unit = {
    val c = advance()
    c match {
      // Single char tokens
      case '(' => addToken(TokenType.LEFT_PAREN)
      case ')' => addToken(TokenType.RIGHT_PAREN)
      case '{' => addToken(TokenType.LEFT_BRACE)
      case '}' => addToken(TokenType.RIGHT_BRACE)
      case '[' => addToken(TokenType.LEFT_BRACKET)
      case ']' => addToken(TokenType.RIGHT_BRACKET)
      case '.' => addToken(TokenType.PERIOD)
      case ',' => addToken(TokenType.COMMA)
      case '=' => addToken(TokenType.EQUAL)
      case ';' => addToken(TokenType.SEMICOLON)
      case '<' => addToken(TokenType.LESS)
      case '*' => addToken(TokenType.STAR)
      case '-' => addToken(TokenType.DASH)
      case '+' => addToken(TokenType.PLUS)

      // Comments
      case '/' => {
        if (isMatch('/')) {
          while (peek() != '\n' && !isAtEnd) {
            advance()
          }
        } else {
          MiniJava.error(line, "Expected comment.")
        }
      }

      case '&' => {
        if (isMatch('&')) {
          addToken(TokenType.AND)
        } else {
          MiniJava.error(line, "Expected 2nd '&'.")
        }
      }

      // Ignore whitespace
      case ' ' | '\t' | '\r' => {}

      // Add to line count
      case '\n' => line += 1

      case '"' => string()

      case _ => {
        if (isDigit(c)) {
          integer()
        } else if (isAlpha(c)) {
          identifier()
        }
      }
    }
  }

  private def addToken(typ: TokenType): Unit = {
    val text = source.substring(start, current)
    buffer.addOne(Token(typ, text, line))
  }

  def tokenize(): List[Token] = {
    while (!isAtEnd) {
      start = current

      scanToken()
    }

    buffer.addOne(Token(TokenType.EOF, "", line))

    buffer.toList
  }
}
