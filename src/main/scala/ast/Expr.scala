package minijava
package ast

import tokenizer.Token

/** Things that produce values of use
  */
sealed abstract class Expr {
  def accept[R](visitor: Expr.Visitor[R]): R
}

object Expr {
  trait Visitor[R] {
    def visitBinary(expr: Binary): R = ???
    def visitIndexGet(expr: IndexGet): R = ???
    def visitGetLength(expr: GetLength): R = ???
    def visitCall(expr: Call): R = ???
    def visitInteger(expr: Integer): R = ???
    def visitBool(expr: Bool): R = ???
    def visitVariable(expr: Variable): R = ???
    def visitThis(expr: This): R = ???
    def visitNewArray(expr: NewArray): R = ???
    def visitNewInstance(expr: NewInstance): R = ???
    def visitUnary(expr: Unary): R = ???
    def visitGrouping(expr: Grouping): R = ???
  }

  // foo <op> bar
  case class Binary(left: Expr, op: Token, opKind: BinaryOp, right: Expr) extends Expr {
    override def accept[R](visitor: Visitor[R]): R = visitor.visitBinary(this)
  }

  // foo[<expr>]
  case class IndexGet(name: Token, obj: Expr, index: Expr) extends Expr {
    override def accept[R](visitor: Visitor[R]): R = visitor.visitIndexGet(this)
  }

  // foo.length
  case class GetLength(name: Token, obj: Expr) extends Expr {
    override def accept[R](visitor: Visitor[R]): R = visitor.visitGetLength(this)
  }

  // foo.bar(<args>)
  case class Call(calle: Expr, name: Token, args: List[Expr] = List.empty) extends Expr {
    override def accept[R](visitor: Visitor[R]): R = visitor.visitCall(this)
  }

  // 123, true, false
  case class Integer(literal: Token, value: Int) extends Expr {
    override def accept[R](visitor: Visitor[R]): R = visitor.visitInteger(this)
  }
  case class Bool(literal: Token, value: Boolean) extends Expr {
    override def accept[R](visitor: Visitor[R]): R = visitor.visitBool(this)
  }
  // bonus: "foo"?
  case class String(literal: Token, value: String) extends Expr {
    override def accept[R](visitor: Visitor[R]): R = ???
  }

  // let foo = ...;
  // foo (used in some context)
  case class Variable(name: Token) extends Expr {
    override def accept[R](visitor: Visitor[R]): R = visitor.visitVariable(this)
  }

  // this (used in some context)
  case class This(keyword: Token) extends Expr {
    override def accept[R](visitor: Visitor[R]): R = visitor.visitThis(this)
  }

  // new Foo[<amount>]
  case class NewArray(typ: Token, count: Expr) extends Expr {
    override def accept[R](visitor: Visitor[R]): R = visitor.visitNewArray(this)
  }

  // new Foo(...)
  case class NewInstance(typ: Token, args: List[Expr] = List.empty) extends Expr {
    override def accept[R](visitor: Visitor[R]): R = visitor.visitNewInstance(this)
  }

  // <op>foo, !foo, -foo
  case class Unary(op: Token, opKind: UnaryOp, left: Expr) extends Expr {
    override def accept[R](visitor: Visitor[R]): R = visitor.visitUnary(this)
  }

  // (foo)
  case class Grouping(inner: Expr) extends Expr {
    override def accept[R](visitor: Visitor[R]): R = visitor.visitGrouping(this)
  }
}
