package minijava
package visitor

import ast.{Program, Stmt, Type}
import visitor.TypeInfo.ClassType

import minijava.tokenizer.Token

import scala.collection.immutable
import scala.collection.mutable
import minijava.ast.Expr
import minijava.visitor.TypeInfo.Bool
import minijava.visitor.TypeInfo.IntArray
import minijava.visitor.TypeInfo.Unknown

sealed trait TypeInfo {
  def coerce(other: TypeInfo, typeTable: Map[String, TypeInfo]): Option[TypeInfo] = {
    if (this == other) {
      return Some(this)
    }

    if (this == TypeInfo.Unknown || other == TypeInfo.Unknown) {
      return Some(TypeInfo.Unknown)
    }

    if (this.isPrimitive || other.isPrimitive) {
      return None
    }

    this match
      case c: ClassType =>
        c.parent match
          case Some(value) =>
            val parentType = typeTable.get(value).get
            return parentType.coerce(other, typeTable)
          case None =>

      case _ =>

    None
  }

  def isPrimitive: Boolean = {
    this match
      case TypeInfo.Bool                                         => true
      case TypeInfo.Int                                          => true
      case TypeInfo.IntArray                                     => true
      case TypeInfo.Void                                         => true
      case TypeInfo.Unknown                                      => false
      case TypeInfo.ClassType(name, parent, properties, methods) => false
  }

  override def toString(): String = this match
    case TypeInfo.Bool => "boolean"
    case TypeInfo.Int => "int"
    case TypeInfo.IntArray => "int[]"
    case TypeInfo.Void => "void"
    case TypeInfo.Unknown => "unknown"
    case TypeInfo.ClassType(name, parent, properties, methods) => name.lexeme

}

object TypeInfo {
  // Primitive types
  case object Bool extends TypeInfo
  case object Int extends TypeInfo
  case object IntArray extends TypeInfo
  case object Void extends TypeInfo
  case object Unknown extends TypeInfo // used when type check for something fails

  case class ClassType(
      name: Token,
      // NOTE: Use placeholder type while we search for new type?
      // If at the end of the typechecking phase, that type is still placeholder, mark it as error
      parent: Option[String],
      properties: List[PropertyDeclaration],
      methods: List[MethodDeclaration]
  ) extends TypeInfo
}

case class Argument(typ: String, name: Token)

// Typenames are held as strings to then be looked up in the type table
case class PropertyDeclaration(typ: String, name: Token, prop: Stmt.Property)
case class MethodDeclaration(
    returnType: String,
    name: Token,
    args: List[Argument],
    meth: Stmt.Method
)

/** Visit declarations of classes and methods and collect them into a type table.
  *
  * This is a separate pass before proper type checking is done
  */
class TypeCollectorVisitor extends Stmt.Visitor[Unit] with Expr.Visitor[Unit] {
  private val typeTable = mutable.HashMap[String, TypeInfo]()

  // primitive types
  typeTable.put("int", TypeInfo.Int)
  typeTable.put("int[]", TypeInfo.IntArray)
  typeTable.put("boolean", TypeInfo.Bool)
  typeTable.put("void", TypeInfo.Void)

  override def visitMainClass(mainClass: Stmt.MainClass): Unit = {
    typeTable.put(
      mainClass.name.lexeme,
      ClassType(
        mainClass.name,
        None,
        List.empty,
        List.empty // FIXME: Include main method?
      )
    )
  }

  override def visitClass(cls: Stmt.Class): Unit = {
    // Check for duplicate properties
    cls.properties
      .map(_.name)
      .groupBy(_.lexeme) // Group properties by name
      .filter(_._2.length > 1) // Get only those that duplicate
      .foreach(dup => { // For each duplicate, create an error
        val tokens = cls.properties
          .filter(_.name.lexeme == dup._1)
          .map(_.name)
          .drop(1) // Drop the first one because that property was well... first. Shouldn't be considered a duplicate.
        for (tok <- tokens) {
          MiniJava.error(tok, s"Duplicate property `${dup._1}` in class ${cls.name.lexeme}.")
        }
      })

    val properties = cls.properties.map(prop => {
      val t = prop.typ.toString
      PropertyDeclaration(t, prop.name, prop)
    })

    val methods = cls.methods.map(meth => {
      // FIXME: Only collects top-level variable declarations
      val varStmts = meth.body.statements
        .filter(s => s.isInstanceOf[Stmt.Property])
        .map(s => s.asInstanceOf[Stmt.Property])

      val combinedNames = (meth.arguments.map(_.name) ++ varStmts.map(_.name)).groupBy(_.lexeme)
      for (named <- combinedNames) {
        if (named._2.length > 1) {
          for (variable <- named._2.drop(1)) {
            MiniJava.error(
              variable,
              s"Duplicate declaration of variable ${variable.lexeme} in method ${meth.name.lexeme} of class ${cls.name.lexeme}."
            )
          }
        }
      }

      MethodDeclaration(
        meth.returnType.toString,
        meth.name,
        meth.arguments.map(a => Argument(a.typ.toString, a.name)),
        meth
      )
    })

    val classType = TypeInfo.ClassType(
      cls.name,
      cls.parentClass.map(_.name.lexeme),
      properties,
      methods
    )

    val clsName = cls.name.lexeme
    if (typeTable.contains(clsName)) {
      MiniJava.error(cls.name, s"Duplicate class $clsName.")
    }

    typeTable.put(clsName, classType)
  }

  private def circularityCheck(
      start: TypeInfo.ClassType,
      c: TypeInfo.ClassType,
      visited: List[String]
  ): Unit = {
    val newVisited = c.name.lexeme :: visited

    c.parent match
      case Some(parentName) =>
        if (newVisited.contains(parentName)) {
          MiniJava.error(
            c.name,
            s"Circular inheritance detected for class ${c.name.lexeme} extending $parentName"
          )
        } else {
          typeTable.get(parentName) match
            case Some(value) =>
              value match {
                case classType: ClassType =>
                  circularityCheck(start, classType, newVisited)
                case _ =>
              }
            case None =>
              if (start.name.lexeme == c.name.lexeme) {
                MiniJava.error(c.name.line, s"Parent class $parentName does not exist.")
              }
        }
      case None => // No parent class, do nothing
  }

  // Returns type table and list of type errors (can be empty)
  def getTypeTable(program: Program): immutable.Map[String, TypeInfo] = {
    visitMainClass(program.mainClass)
    for (c <- program.classes) {
      visitClass(c)
    }

    for (typ <- typeTable.values) {
      typ match
        case c: ClassType => circularityCheck(c, c, List())
        case _            =>
    }

    typeTable.toMap
  }
}
