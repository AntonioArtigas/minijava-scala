package minijava
package ast

import tokenizer.TokenType

object BinaryOp {
  def fromTokenType(typ: TokenType): BinaryOp = {
    typ match {
      case TokenType.AND => AND
      case TokenType.DASH => DASH
      case TokenType.PLUS => PLUS
      case TokenType.LESS => LESS_THAN
      case TokenType.STAR => MUL
      case _ => ???
    }
  }
}

enum BinaryOp {
  case

  AND, // &&
  DASH, // -
  PLUS, // +
  MUL, // *
  LESS_THAN, // <
  GREATER_THAN, // >
  GREATER_EQUAL, // >=
  LESS_EQUAL // <=
}
