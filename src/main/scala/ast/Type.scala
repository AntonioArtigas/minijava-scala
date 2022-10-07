package minijava
package ast

sealed trait Type

/**
 * Represents a type within the AST
 */
object Type {
  case object Int extends Type
  case object IntArray extends Type
  case object Bool extends Type
  case class Custom(name: String) extends Type
}
