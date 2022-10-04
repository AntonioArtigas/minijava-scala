package ast

sealed class Type {
  case class Int() extends Type
  case class IntArray() extends Type
  case class Bool() extends Type
  case class String() extends Type
  case class Custom(name: String) extends Type
}
