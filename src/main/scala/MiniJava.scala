package minijava

import ast.{Expr, Stmt}
import parser.ParserV1
import tokenizer.{Token, TokenType, Tokenizer}
import visitor.{CodeGenerationVisitor, TypeCheckerVisitor, TypeCollectorVisitor}

import java.nio.file.{Files, Path}

object MiniJava {
  // If this flag is true, we stop after finishing a stage. Like tokenizing, parsing, or analysis, etc.
  var hadError = false
  var debug = false

  def error(line: Int, message: String): Unit = {
    report(line, "", message)
  }

  def error(token: Token, message: String): Unit = {
    if (token.typ == TokenType.EOF) {
      report(token.line, " at end", message)
    } else {
      report(token.line, s" at '${token.lexeme}'", message)
    }
  }

  def report(line: Int, where: String, message: String): Unit = {
    System.err.println(s"[line $line] Error$where: $message")
    hadError = true
  }

  def main(args: Array[String]): Unit = {
    val source = Files.readString(Path.of("tests", "Factorial.mj"))

    val tokenizer = new Tokenizer(source)
    val tokens = tokenizer.tokenize()
    if (hadError) {
      return
    }

    val parser = new ParserV1(tokens)
    val program = parser.parse()

    if (debug) {
      println(program)
    }

    if (hadError) {
      return
    }

    val typeCollector = TypeCollectorVisitor()
    val typeTable = typeCollector.getTypeTable(program)

    if (debug) {
      println(typeTable)
    }

    val typeSanityChecker = TypeSanityChecker(typeTable)
    typeSanityChecker.checkTypeTable()

    val typeChecker = TypeCheckerVisitor(typeTable)
    typeChecker.typecheckProgram(program)
    if (hadError) {
      return
    }

    val codeGen = CodeGenerationVisitor(typeTable)

    codeGen.compileProgram(program, "tmp")
  }
}
