package minijava
package ast

import minijava.ast.Type.IntArray

import minijava.ast.Type.Bool

import minijava.ast.Type.Custom

sealed trait Type {
  override def toString: String = this match
    case Type.Int          => "int"
    case Type.IntArray     => "int[]"
    case Type.Bool         => "boolean"
    case Type.Custom(name) => name

  def toTypeDescriptor: String = this match
    case Type.Int => "I"
    case Type.IntArray => "[I"
    case Type.Bool => "Z"
    case Type.Custom(name) => s"L$name;"

}

/** Represents a type within the AST
  */
object Type {
  case object Int extends Type
  case object IntArray extends Type
  case object Bool extends Type
  case class Custom(name: String) extends Type

  def fromString(str: String): Type = str match {
    case "int"     => Type.Int
    case "boolean" => Type.Bool
    case _         => Type.Custom(str)
    // FIXME: There's a hack in the parser to deal with array types
  }
}
