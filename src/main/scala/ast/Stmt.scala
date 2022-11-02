package minijava
package ast

import tokenizer.Token

/** Things that modify state or declare things
  */
sealed abstract class Stmt {
  def accept[R](visitor: Stmt.Visitor[R]): R
}

object Stmt {
  trait Visitor[R] {
    def visitClass(stmt: Class): R = ???
    def visitMainClass(stmt: MainClass): R = ???
    def visitMethod(stmt: Method): R = ???
    def visitMainMethod(stmt: MainMethod): R = ???
    def visitProperty(stmt: Property): R = ???
    def visitBlock(stmt: Block): R = ???
    def visitAssign(stmt: Assign): R = ???
    def visitIf(stmt: If): R = ???
    def visitWhile(stmt: While): R = ???
    def visitPrint(stmt: Print): R = ???
    def visitIndexSet(stmt: IndexSet): R = ???
    def visitReturn(stmt: Return): R = ???
  }

  // Declarations
  case class Class(
      name: Token,
      parentClass: Option[Expr.Variable] = None,
      methods: List[Method] = List.empty,
      properties: List[Property] = List.empty
  ) extends Stmt {
    override def accept[R](visitor: Visitor[R]): R = visitor.visitClass(this)
  }

  case class MainClass(name: Token, body: MainMethod) extends Stmt {
    override def accept[R](visitor: Visitor[R]): R = visitor.visitMainClass(this)
  }

  case class Method(
      returnType: Type,
      name: Token,
      arguments: List[Argument] = List.empty,
      body: Block
  ) extends Stmt {
    override def accept[R](visitor: Visitor[R]): R = visitor.visitMethod(this)
  }

  case class MainMethod(argName: Token, body: Block) extends Stmt {
    override def accept[R](visitor: Visitor[R]): R = visitor.visitMainMethod(this)
  }

  // TODO: Rename because we also use these in methods
  case class Property(typ: Type, name: Token, assigned: Option[Expr] = None) extends Stmt {
    override def accept[R](visitor: Visitor[R]): R = visitor.visitProperty(this)
  }

  // Other
  // { <stmt>* }
  case class Block(statements: List[Stmt] = List.empty) extends Stmt {
    override def accept[R](visitor: Visitor[R]): R = visitor.visitBlock(this)
  }

  case class Assign(name: Token, assigned: Expr) extends Stmt {
    override def accept[R](visitor: Visitor[R]): R = visitor.visitAssign(this)
  }

  // if (<cond>) <stmt>
  case class If(
      keyword: Token,
      condition: Expr,
      thenBranch: Stmt,
      elseBranch: Option[Stmt] = None
  ) extends Stmt {
    override def accept[R](visitor: Visitor[R]): R = visitor.visitIf(this)
  }

  // while (<condition>) <stmt>
  case class While(keyword: Token, condition: Expr, body: Stmt) extends Stmt {
    override def accept[R](visitor: Visitor[R]): R = visitor.visitWhile(this)
  }

  // System.out.println(<expr>)
  case class Print(start: Token, expression: Expr) extends Stmt {
    override def accept[R](visitor: Visitor[R]): R = visitor.visitPrint(this)
  }

  // foo["bar"] = baz;
  case class IndexSet(obj: Token, index: Expr, value: Expr) extends Stmt {
    override def accept[R](visitor: Visitor[R]): R = visitor.visitIndexSet(this)
  }

  case class Return(keyword: Token, expr: Expr) extends Stmt {
    override def accept[R](visitor: Visitor[R]): R = visitor.visitReturn(this)
  }
}
