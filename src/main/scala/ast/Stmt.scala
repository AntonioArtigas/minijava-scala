package minijava
package ast

import tokenizer.Token

/** Things that modify state or declare things
  */
sealed abstract class Stmt

object Stmt {
  // Declarations
  case class Class(
      name: Token,
      parentClass: Option[Expr.Variable] = None,
      methods: List[Method] = List.empty,
      properties: List[Property] = List.empty
  ) extends Stmt

  case class MainClass(name: Token, body: MainMethod) extends Stmt

  case class Method(returnType: Type, name: Token, arguments: List[Argument] = List.empty, body: Block)
      extends Stmt

  case class MainMethod(argName: Token, body: Block) extends Stmt

  case class Property(typ: Type, name: Token, assigned: Option[Expr] = None) extends Stmt

  // Other
  // { <stmt>* }
  case class Block(statements: List[Stmt] = List.empty) extends Stmt

  case class Assign(name: Token, assigned: Expr) extends Stmt

  // if (<cond>) <stmt>
  case class If(
      condition: Expr,
      thenBranch: Stmt,
      elseBranch: Option[Stmt] = None
  ) extends Stmt

  // while (<condition>) <stmt>
  case class While(condition: Expr, body: Stmt) extends Stmt

  // System.out.println(<expr>)
  case class Print(expression: Expr) extends Stmt

  // foo["bar"] = baz;
  case class IndexSet(obj: Token, index: Expr, value: Expr) extends Stmt

  case class Return(expr: Expr) extends Stmt
}
