package minijava
package ast

import tokenizer.Token

/** Things that produce values of use
  */
sealed abstract class Expr

object Expr {
  // foo <op> bar
  case class Binary(left: Expr, op: BinaryOp, right: Expr) extends Expr

  // foo[<expr>]
  case class IndexGet(obj: Expr, index: Expr) extends Expr

  // foo.length
  case class GetLength(obj: Expr) extends Expr

  // foo.bar(<args>)
  case class Call(calle: Expr, name: Token, args: List[Expr] = List.empty)
      extends Expr

  // 123, true, false
  case class Integer(value: Int) extends Expr
  case class Bool(value: Boolean) extends Expr
  // bonus: "foo"?
  case class String(value: String) extends Expr

  // let foo = ...;
  // foo (used in some context)
  case class Variable(name: Token) extends Expr

  // this (used in some context)
  case class This(keyword: Token) extends Expr

  // new Foo[<amount>]
  case class NewArray(typ: Token, count: Expr) extends Expr

  // new Foo(...)
  case class NewInstance(typ: Token, args: List[Expr] = List.empty) extends Expr

  // <op>foo, !foo, -foo
  case class Unary(op: UnaryOp, left: Expr) extends Expr

  // (foo)
  case class Grouping(inner: Expr) extends Expr
}
