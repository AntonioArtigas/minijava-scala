package minijava

import ast.{Expr, Stmt}
import tokenizer.{Token, TokenType}

import tokenizer.Tokenizer
import parser.ParserV1

import java.nio.file.{Files, Path}

object MiniJava {
  // If this flag is true, we stop after finishing a stage. Like tokenizing, parsing, or analysis, etc.
  var hadError = false

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
    val source = Files.readString(Path.of("tests", "Bad.mj"))

    val tokenizer = new Tokenizer(source)

    val tokens = tokenizer.tokenize()

    val parser = new ParserV1(tokens)

    val program = parser.parse()

    println(program)
  }
}
