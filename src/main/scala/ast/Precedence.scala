package minijava
package ast

enum Precedence(val value: Int) {
  def canAssign: Boolean = value == NONE.value

  def next: Precedence = {
    this match {
      case NONE    => AND
      case AND     => COMPARE
      case COMPARE => TERM
      case TERM    => FACTOR
      case FACTOR  => UNARY
      case UNARY   => CALL
      case CALL    => PRIMARY
      case PRIMARY => PRIMARY
    }
  }

  def previous: Precedence = {
    this match {
      case NONE    => NONE
      case AND     => NONE
      case COMPARE => AND
      case TERM    => COMPARE
      case FACTOR  => TERM
      case UNARY   => FACTOR
      case CALL    => UNARY
      case PRIMARY => CALL
    }
  }

  case NONE extends Precedence(0)
  case AND extends Precedence(1) // &&
  case COMPARE extends Precedence(2) // <
  case TERM extends Precedence(3) // + -
  case FACTOR extends Precedence(4) // *
  case UNARY extends Precedence(5) // !
  case CALL extends Precedence(6) // . ()
  case PRIMARY extends Precedence(7) // foo[idx], 123, true, false
}
