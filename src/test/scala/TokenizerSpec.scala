package minijava

import org.scalatest._
import flatspec._
import matchers._

import tokenizer.{Tokenizer, Token, TokenType}

class TokenizerSpec extends AnyFlatSpec with should.Matchers {
  "A lexer" should "tokenize 2 + 2" in {
    val tokenizer = new Tokenizer("2 + 2")
    val tokens = tokenizer.tokenize()

    val expected = List(
      Token(TokenType.INTEGER, "2", 1),
      Token(TokenType.PLUS, "+", 1),
      Token(TokenType.INTEGER, "2", 1),
      Token(TokenType.EOF, "", 1)
    )

    tokens should contain theSameElementsAs expected
  }
}
