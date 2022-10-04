package minijava
package ast

import tokenizer.TokenType

object UnaryOp {
  def fromTokenType(typ: TokenType): UnaryOp = {
    typ match {
      case TokenType.BANG => BANG
      case _ => ???
    }
  }
}

enum UnaryOp {
  case BANG //, NEGATE
}
