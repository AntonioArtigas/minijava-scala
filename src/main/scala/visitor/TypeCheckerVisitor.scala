package minijava
package visitor

import ast.Stmt.Return
import ast.*
import tokenizer.Token
import visitor.TypeInfo.{Bool, ClassType, IntArray, Unknown}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.{HashMap, ListBuffer, Stack}
import scala.util.control.NonLocalReturns.*

/** Pass over the types listed in the type table and check statements for correct types.
  *
  * @param typeTable
  *   Type table to use
  */
class TypeCheckerVisitor(val typeTable: Map[String, TypeInfo])
    extends Stmt.Visitor[TypeInfo]
    with Expr.Visitor[TypeInfo] {

  private val variableScopes = mutable.Stack[mutable.HashMap[String, Option[TypeInfo]]]()

  private var currentClass: Option[Stmt.Class] = None
  private var currentMethod: Option[Stmt.Method] = None

  override def visitClass(stmt: Stmt.Class): TypeInfo = {
    // Has to be in there, or else we have bigger problems
    currentClass = Some(stmt)

    beginScope()

    val classType = typeTable.getOrElse(stmt.name.lexeme, TypeInfo.Unknown)

    // Put this into scope
    variableScopes.head.put("this", Some(classType))

    // load class properties
    for (prop <- currentClass.get.properties) {
      val propertyType =
        typeTable.getOrElse(prop.typ.toString, errorTypeNotFound(prop.typ.toString, prop.name.line))

      declare(prop.name)
      define(prop.name, propertyType)
    }

    for (meth <- stmt.methods) {
      currentMethod = Some(meth)
      meth.accept(this)
    }

    endScope()

    TypeInfo.Void
  }

  override def visitMainClass(stmt: Stmt.MainClass): TypeInfo = {
    stmt.body.accept(this)

    TypeInfo.Void
  }

  override def visitMethod(stmt: Stmt.Method): TypeInfo = {
    val expectedReturnType = typeTable.get(stmt.returnType.toString)
    if (expectedReturnType.isEmpty) {
      errorTypeNotFound(stmt.returnType.toString, stmt.name.line)
    }

    // Check if types in args exist
    for (arg <- stmt.arguments) {
      val typeName = arg.typ.toString
      if (!typeTable.contains(typeName)) {
        errorTypeNotFound(typeName, arg.name.line)
      }
    }

    val argsWithTypes =
      stmt.arguments.map(arg => (typeTable.getOrElse(arg.typ.toString, TypeInfo.Unknown), arg.name))

    beginScope()

    // Load variables from method into scope
    for (awt <- argsWithTypes) {
      declare(awt._2)
      define(awt._2, awt._1)
    }

    for (s <- stmt.body.statements) {
      s.accept(this)
    }

    if (expectedReturnType.isDefined && !expectedReturnType.get.isInstanceOf[TypeInfo.Void.type]) {
      // If return type of method is defined, check if last statement is return and compare types
      val last = stmt.body.statements.lastOption
      if (last.isDefined) {
        last.get match
          case Return(keyword, expr) =>
            val givenReturnType = expr.accept(this)
            if (expectedReturnType.get.coerce(givenReturnType, typeTable).isEmpty) {
              errorExpectedType(givenReturnType, expectedReturnType.get, keyword.line)
            }
          case _ => errorReturnRequired(stmt.name, expectedReturnType.get.toString)
      } else {
        errorReturnRequired(stmt.name, expectedReturnType.get.toString)
      }
    }

    endScope()

    TypeInfo.Void
  }

  override def visitMainMethod(stmt: Stmt.MainMethod): TypeInfo = {
    beginScope()

    for (s <- stmt.body.statements) {
      s.accept(this)
    }

    endScope()

    TypeInfo.Void
  }

  override def visitProperty(stmt: Stmt.Property): TypeInfo = {
    declare(stmt.name)
    val typ = typeTable.getOrElse(
      stmt.typ.toString,
      errorTypeNotFound(stmt.typ.toString, stmt.name.line)
    )
    define(stmt.name, typ)

    typ
  }

  override def visitBlock(stmt: Stmt.Block): TypeInfo = {
    beginScope()
    for (s <- stmt.statements) {
      s.accept(this)
    }
    endScope()

    TypeInfo.Void
  }

  override def visitAssign(stmt: Stmt.Assign): TypeInfo = {
    val wantedType = resolveLocal(stmt.name)
    val exprType = stmt.assigned.accept(this)

    if (wantedType.coerce(exprType, typeTable).isDefined) {
      wantedType
    } else {
      errorExpectedType(wantedType, exprType, stmt.name.line)
    }
  }

  override def visitIf(stmt: Stmt.If): TypeInfo = {
    val conditionType = stmt.condition.accept(this)

    conditionType match
      case Bool =>
      case _    => errorExpectedType(conditionType, TypeInfo.Bool, stmt.keyword.line)

    stmt.thenBranch.accept(this)
    stmt.elseBranch.map(_.accept(this))

    TypeInfo.Void
  }

  override def visitWhile(stmt: Stmt.While): TypeInfo = {
    val conditionType = stmt.condition.accept(this)

    conditionType match
      case Bool =>
      case _    => errorExpectedType(conditionType, TypeInfo.Bool, stmt.keyword.line)

    stmt.body.accept(this)

    TypeInfo.Void
  }

  override def visitPrint(stmt: Stmt.Print): TypeInfo = {
    val exprType = stmt.expression.accept(this)

    if (!exprType.isInstanceOf[TypeInfo.Int.type]) {
      errorExpectedType(exprType, TypeInfo.Int, stmt.start.line)
    }

    TypeInfo.Void
  }

  override def visitIndexSet(stmt: Stmt.IndexSet): TypeInfo = {
    val indexType = stmt.index.accept(this)

    indexType match
      case TypeInfo.Int =>
      case _            => errorExpectedType(indexType, TypeInfo.Int, stmt.obj.line)

    TypeInfo.Void
  }

  override def visitReturn(stmt: Stmt.Return): TypeInfo = stmt.expr.accept(this)

  override def visitBinary(expr: Expr.Binary): TypeInfo = {
    val leftType = expr.left.accept(this)
    val rightType = expr.right.accept(this)
    val op = expr.opKind

    op match
      // Int only operations
      case BinaryOp.DASH | BinaryOp.PLUS | BinaryOp.MUL =>
        val leftIsInt = leftType.coerce(TypeInfo.Int, typeTable)
        val rightIsInt = rightType.coerce(TypeInfo.Int, typeTable)

        if (!(leftIsInt.isDefined && rightIsInt.isDefined)) {
          errorOperatorTypesWrong(expr.op, expr.opKind, leftType, rightType)
        } else {
          TypeInfo.Int
        }

      case BinaryOp.LESS_EQUAL | BinaryOp.LESS_THAN | BinaryOp.GREATER_THAN |
          BinaryOp.GREATER_EQUAL =>
        val leftIsInt = leftType.coerce(TypeInfo.Int, typeTable)
        val rightIsInt = rightType.coerce(TypeInfo.Int, typeTable)

        if (!(leftIsInt.isDefined && rightIsInt.isDefined)) {
          errorOperatorTypesWrong(expr.op, expr.opKind, leftType, rightType)
        } else {
          TypeInfo.Bool
        }

      // Boolean only operations
      case BinaryOp.AND =>
        val leftIsBool = leftType.coerce(TypeInfo.Bool, typeTable)
        val rightIsBool = rightType.coerce(TypeInfo.Bool, typeTable)

        if (!(leftIsBool.isDefined && rightIsBool.isDefined)) {
          errorOperatorTypesWrong(expr.op, expr.opKind, leftType, rightType)
        } else {
          TypeInfo.Bool
        }
  }

  override def visitIndexGet(expr: Expr.IndexGet): TypeInfo = {
    val indexType = expr.index.accept(this)

    if (indexType.coerce(TypeInfo.Int, typeTable).isEmpty) {
      return errorExpectedType(indexType, TypeInfo.Int, expr.name.line)
    }

    TypeInfo.Int
  }

  override def visitGetLength(expr: Expr.GetLength): TypeInfo = {
    val objType = expr.obj.accept(this)

    objType match {
      case TypeInfo.IntArray => TypeInfo.Int
      case _                 => errorExpectedType(objType, TypeInfo.IntArray, expr.name.line)
    }
  }

  override def visitCall(expr: Expr.Call): TypeInfo = {
    val objType = expr.calle.accept(this)

    objType match
      case c: ClassType =>
        val argTypes = expr.args.map(_.accept(this))

        val matchedMethod = getMatchingMethod(c, expr.name.lexeme, argTypes)
        if (matchedMethod.isDefined) {
          expr.method = matchedMethod
          expr.clsName = Some(c.name.lexeme)
          typeTable(matchedMethod.get.returnType)
        } else {
          TypeInfo.Unknown
        }
      case TypeInfo.Unknown => TypeInfo.Unknown
      case _                => errorNotAnObject(objType, expr.name)
  }

  override def visitInteger(expr: Expr.Integer): TypeInfo = TypeInfo.Int

  override def visitBool(expr: Expr.Bool): TypeInfo = TypeInfo.Bool

  // Tries to find the "closest" declared variable, whether that'd be a local variable or class property
  private def varType(name: String): Option[TypeInfo] = returning {
    for (scope <- variableScopes) {
      val variable = scope.getOrElse(name, None)
      if (variable.isDefined) {
        throwReturn(variable)
      }
    }

    None
  }

  override def visitVariable(expr: Expr.Variable): TypeInfo = {
    val typ = varType(expr.name.lexeme)
    if (typ.isEmpty) {
      errorVariableNotFound(expr.name)
    } else {
      typ.get
    }
  }

  // Should not be null
  override def visitThis(expr: Expr.This): TypeInfo =
    typeTable.getOrElse(currentClass.get.name.lexeme, TypeInfo.Unknown)

  override def visitNewArray(expr: Expr.NewArray): TypeInfo = {
    val sizeType = expr.count.accept(this)

    if (!sizeType.isInstanceOf[TypeInfo.Int.type]) {
      errorExpectedType(sizeType, TypeInfo.Int, expr.typ.line)
      return TypeInfo.Unknown
    }

    TypeInfo.IntArray
  }

  override def visitNewInstance(expr: Expr.NewInstance): TypeInfo = {
    typeTable.getOrElse(
      expr.typ.lexeme,
      errorTypeNotFound(expr.typ)
    )
  }

  override def visitUnary(expr: Expr.Unary): TypeInfo = {
    val exprType = expr.left.accept(this)

    expr.opKind match
      case UnaryOp.BANG =>
        exprType match {
          case TypeInfo.Bool => TypeInfo.Bool
          case _             => errorExpectedType(exprType, TypeInfo.Bool, expr.op.line)
        }
      case UnaryOp.NEGATE =>
        exprType match
          case TypeInfo.Int => TypeInfo.Int
          case _            => errorExpectedType(exprType, TypeInfo.Int, expr.op.line)
  }

  override def visitGrouping(expr: Expr.Grouping): TypeInfo = expr.inner.accept(this)

  @tailrec
  private def getMatchingMethod(
      c: TypeInfo.ClassType,
      methodName: String,
      argTypes: List[TypeInfo]
  ): Option[MethodDeclaration] = {
    val potentialMethods =
      c.methods.filter(_.name.lexeme == methodName)

    val matchingMethods = potentialMethods.filter(p => {
      val matchTypes = p.args.map(a => typeTable(a.typ))

      // quick check: make sure arity matches
      if (matchTypes.length == argTypes.length) {
        val zipped = matchTypes.zip(argTypes)
        val matches = zipped.map((l, r) => l.coerce(r, typeTable).isDefined)

        matches.isEmpty || !matches.contains(false)
      } else {
        false
      }
    })

    // If the current class doesn't have the method, check its parent class
    matchingMethods.headOption match
      case None =>
        c.parent match
          case Some(value) =>
            getMatchingMethod(
              typeTable(value).asInstanceOf[TypeInfo.ClassType],
              methodName,
              argTypes
            )
          case None => None

      case Some(value) => Some(value)
  }

  private def errorExpectedType(givenType: TypeInfo, wantedType: TypeInfo, line: Int): TypeInfo = {
    MiniJava.error(line, s"Expected $wantedType but was $givenType")
    TypeInfo.Unknown
  }

  private def errorTypeNotFound(name: Token): TypeInfo = {
    MiniJava.error(name, s"Cannot resolve type $name.")
    TypeInfo.Unknown
  }

  private def errorTypeNotFound(name: String, line: Int): TypeInfo = {
    MiniJava.error(line, s"Cannot resolve type $name.")
    TypeInfo.Unknown
  }

  private def errorNotAnObject(t: TypeInfo, methodName: Token): TypeInfo = {
    MiniJava.error(methodName, s"Cannot call method $methodName on type $t")
    TypeInfo.Unknown
  }

  private def errorOperatorTypesWrong(
      op: Token,
      opKind: BinaryOp,
      leftType: TypeInfo,
      rightType: TypeInfo
  ): TypeInfo = {
    MiniJava.error(op, s"Cannot use operator $opKind with types $leftType and $rightType.")
    TypeInfo.Unknown
  }

  private def errorVariableNotFound(name: Token): TypeInfo = {
    MiniJava.error(name, s"Variable ${name.lexeme} is not defined")
    TypeInfo.Unknown
  }

  private def errorReturnRequired(methodName: Token, typ: String): TypeInfo = {
    MiniJava.error(
      methodName,
      s"Method ${methodName.lexeme} requires return of $typ at end of body."
    )
    TypeInfo.Unknown
  }

  private def errorVariableAlreadyDefined(name: Token): Unit = {
    MiniJava.error(name, s"Variable already named ${name.lexeme} in scope.")
  }

  private def declare(name: Token): Unit = {
    if (variableScopes.isEmpty) {
      return
    }

    val scope = variableScopes.top // top is peek?
    if (scope.contains(name.lexeme)) {
      errorVariableAlreadyDefined(name)
    }
    scope.put(name.lexeme, None)
  }

  private def define(name: Token, typ: TypeInfo): Unit = {
    if (variableScopes.isEmpty) {
      return
    }

    variableScopes.top.put(name.lexeme, Some(typ))
  }

  private def beginScope(): Unit = variableScopes.push(mutable.HashMap())

  private def endScope(): Unit = variableScopes.pop()

  private def resolveLocal(name: Token): TypeInfo = returning {
    for (layer <- variableScopes) {
      if (layer.contains(name.lexeme)) {
        throwReturn(layer(name.lexeme).get)
      }
    }

    errorVariableNotFound(name)

    TypeInfo.Unknown
  }

  def typecheckProgram(program: Program): Unit = {
    for (cls <- program.classes) {
      cls.accept(this)
    }

    program.mainClass.accept(this)
  }
}
