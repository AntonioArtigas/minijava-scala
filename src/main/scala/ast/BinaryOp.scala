package minijava
package ast

import tokenizer.TokenType

object BinaryOp {
  def fromTokenType(typ: TokenType): BinaryOp = {
    typ match {
      case TokenType.AND  => AND
      case TokenType.DASH => DASH
      case TokenType.PLUS => PLUS
      case TokenType.LESS => LESS_THAN
      case TokenType.STAR => MUL
      case _              => ???
    }
  }
}

enum BinaryOp {
  case AND, // &&
    DASH, // -
    PLUS, // +
    MUL, // *
    LESS_THAN, // <
    GREATER_THAN, // >
    GREATER_EQUAL, // >=
    LESS_EQUAL // <=

  override def toString(): String = {
    this match
      case BinaryOp.AND           => "&&"
      case BinaryOp.DASH          => "-"
      case BinaryOp.PLUS          => "+"
      case BinaryOp.MUL           => "*"
      case BinaryOp.LESS_THAN     => "<"
      case BinaryOp.GREATER_THAN  => ">"
      case BinaryOp.GREATER_EQUAL => ">="
      case BinaryOp.LESS_EQUAL    => "<="

  }
}
