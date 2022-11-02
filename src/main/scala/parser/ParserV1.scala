package minijava
package parser

import scala.util.control.NonLocalReturns.*
import scala.util.control.Breaks.*
import scala.annotation.unused
import scala.collection.mutable.ListBuffer
import ast.{Argument, BinaryOp, Expr, Precedence, Program, Stmt, Type, UnaryOp}
import tokenizer.{Token, TokenType}

// NOTE: When parsing method bodies, if an error occurs, we sync to a new class declaration and start re-parsing at the class level
/** First implementation of parser Recursive descent parser that uses pratt parsing to parse
  * expressions.
  *
  * "If recursive descent is peanut butter, Pratt parsing is the jelly." - Bob Nystrom
  *
  * @param tokens
  *   Tokens to build an AST from
  */
class ParserV1(val tokens: List[Token]) extends Parser {
  private type PrefixFn = Token => Expr
  private type InfixFn = (Expr, Token) => Expr
  private case class ParseRule(
      prefix: PrefixFn | Null,
      infix: InfixFn | Null,
      precedence: Precedence
  )

  private val rules = Map(
    TokenType.PERIOD -> ParseRule(null, dot, Precedence.PRIMARY),
    TokenType.LEFT_PAREN -> ParseRule(grouping, null, Precedence.PRIMARY),
    TokenType.LEFT_BRACKET -> ParseRule(null, indexGet, Precedence.PRIMARY),
    TokenType.NEW -> ParseRule(instance, null, Precedence.PRIMARY),
    TokenType.DASH -> ParseRule(unary, binary, Precedence.UNARY),
    TokenType.PLUS -> ParseRule(null, binary, Precedence.TERM),
    TokenType.STAR -> ParseRule(null, binary, Precedence.FACTOR),
    TokenType.LESS -> ParseRule(null, binary, Precedence.COMPARE),
    TokenType.BANG -> ParseRule(unary, null, Precedence.UNARY),
    TokenType.AND -> ParseRule(null, binary, Precedence.AND),
    TokenType.IDENTIFIER -> ParseRule(variable, null, Precedence.NONE),
    TokenType.INTEGER -> ParseRule(number, null, Precedence.NONE),
    TokenType.TRUE -> ParseRule(bool, null, Precedence.NONE),
    TokenType.FALSE -> ParseRule(bool, null, Precedence.NONE)
  ).withDefaultValue(ParseRule(null, null, Precedence.NONE))

  class ParseError extends RuntimeException

  private var current = 0

  private def peek(): Token = tokens(current)

  private def previous(): Token = tokens(current - 1)

  private def isAtEnd: Boolean = peek().typ == TokenType.EOF

  private def error(token: Token, message: String): ParseError = {
    MiniJava.error(token, message)
    ParseError()
  }

  /** We're in panic mode! Get to a synchronization token.
    */
  def synchronize(): Unit = {
    advance()

    while (!isAtEnd) {
      if (previous().typ == TokenType.SEMICOLON) {
        return
      }

      peek().typ match {
        case TokenType.CLASS | TokenType.FOR | TokenType.WHILE | TokenType.RETURN => return
        case _ => {} // ignore otherwise
      }

      advance()
    }
  }

  private def advance(): Token = {
    if (!isAtEnd) {
      current += 1
    }
    previous()
  }

  private def check(typ: TokenType): Boolean = {
    if (isAtEnd) {
      return false
    }

    peek().typ == typ
  }

  private def consume(typ: TokenType, message: String): Token = {
    if (check(typ)) {
      return advance()
    }

    throw error(peek(), message)
  }

  // If one of the tokens types is matched against the next one, advance by one and return true, otherwise false
  private def isMatch(types: TokenType*): Boolean = returning {
    // NOTE: Scala for-loops are weird. Need to use this throwReturn function to exit early
    for (typ <- types) {
      if (check(typ)) {
        advance()
        throwReturn(true)
      }
    }

    false
  }

  private def parsePrecedence(precedence: Precedence): Expr | Null = {
    var token = advance()

    // First token we approach should always be some sort of prefix expression
    val prefixRule = rules(previous().typ).prefix
    if (prefixRule == null) {
      throw error(previous(), "Expect expression.")
    }

    var left = prefixRule(token)

    while (precedence.value <= rules(peek().typ).precedence.value) {
      token = advance()
      val infixRule = rules(previous().typ).infix
      left = infixRule(left, token)
    }

    left
  }

  private def grouping(@unused token: Token): Expr = {
    val expr = expression()
    consume(TokenType.RIGHT_PAREN, "Expect ')' after expression.")
    Expr.Grouping(expr)
  }

  private def unary(token: Token): Expr = {
    Expr.Unary(token, UnaryOp.fromTokenType(token.typ), expression())
  }

  private def binary(left: Expr, token: Token): Expr = {
    Expr.Binary(left, token, BinaryOp.fromTokenType(token.typ), expression())
  }

  private def number(token: Token): Expr = Expr.Integer(token, token.lexeme.toInt)
  private def bool(token: Token): Expr = Expr.Bool(token, token.lexeme.toBoolean)
  private def variable(token: Token): Expr = Expr.Variable(token)

  private def dot(left: Expr, token: Token): Expr = {
    val name = consume(TokenType.IDENTIFIER, "Expect identifier after '.'.")

    if (name.lexeme == "length") {
      Expr.GetLength(token, left)
    } else if (isMatch(TokenType.LEFT_PAREN)) {
      val args = ListBuffer[Expr]()
      if (!check(TokenType.RIGHT_PAREN)) {
        while {
          val expr = expression()
          args.addOne(expr)

          isMatch(TokenType.COMMA)
        } do ()
      }

      consume(TokenType.RIGHT_PAREN, "Expect ')' after argument list.")

      Expr.Call(left, name, args.toList)
    } else {
      throw error(name, "Field access is not allowed.")
    }
  }

  private def indexGet(left: Expr, token: Token): Expr = {
    val index = expression()
    consume(TokenType.RIGHT_BRACKET, "Expect ']' after index")
    Expr.IndexGet(token, left, index)
  }

  private def instance(@unused token: Token): Expr = {
    val typ = consume(TokenType.IDENTIFIER, "Expect type after 'new'.")

    if (isMatch(TokenType.LEFT_PAREN)) {
      val args = ListBuffer[Expr]()
      if (!check(TokenType.RIGHT_PAREN)) {
        while {
          val expr = expression()
          args.addOne(expr)

          isMatch(TokenType.COMMA)
        } do ()
      }

      consume(TokenType.RIGHT_PAREN, "Expect ')' after argument list.")

      println(s"NewInstance created for type $typ")
      Expr.NewInstance(typ, args.toList)
    } else if (isMatch(TokenType.LEFT_BRACKET)) {
      val countExpr = expression()
      println(s"NewArray of type $typ with $countExpr")

      consume(TokenType.RIGHT_BRACKET, "Expect '['] after index expression.")

      Expr.NewArray(typ, countExpr)
    } else {
      throw error(peek(), "Unexpected token after identifier.")
    }
  }

  // Kicks off expression parsing, start at precedence above none
  private def expression(): Expr = parsePrecedence(Precedence.AND)

  private def block(): List[Stmt] = {
    val stmts = ListBuffer[Stmt]()

    while (!check(TokenType.RIGHT_BRACE) && !isAtEnd) {
      stmts.addOne(statement())
    }

    consume(TokenType.RIGHT_BRACE, "Expect '}' after block.")

    stmts.toList
  }

  private def mainClassDefinition(): Stmt.MainClass = {
    consume(TokenType.CLASS, "Expect 'class'.")
    val name = consume(TokenType.IDENTIFIER, "Expect name for class.")
    consume(TokenType.LEFT_BRACE, "Expect '{' after class name.")

    var mainMethod: Stmt.MainMethod | Null = null

    // Parse main method
    consume(TokenType.PUBLIC, "Expect 'public' for main method.")
    consume(TokenType.STATIC, "Expect 'static' for main method.")
    val void = consume(TokenType.IDENTIFIER, "Expect 'void' for main method.")
    if (void.lexeme != "void") {
      error(peek(), "Expect 'void' after 'static'.")
    }

    consume(TokenType.IDENTIFIER, "Expect method name.")
    consume(TokenType.LEFT_PAREN, "Expect '('.")
    consume(TokenType.IDENTIFIER, "Expect String type.")
    consume(TokenType.LEFT_BRACKET, "Expect '['")
    consume(TokenType.RIGHT_BRACKET, "Expect ']'")
    val argName = consume(TokenType.IDENTIFIER, "Expect name for parameter.")
    consume(TokenType.RIGHT_PAREN, "Expect ')' after parameter.")
    val body = statement()

    consume(TokenType.RIGHT_BRACE, "Expect '}' after main class body.")

    body match {
      case Stmt.Block(_) => {
        mainMethod = Stmt.MainMethod(argName, body.asInstanceOf[Stmt.Block])
        Stmt.MainClass(name, mainMethod)
      }

      case _ => throw error(peek(), "Expected block after main method declaration.")
    }
  }

  private def method(): Stmt.Method = {
    val returnType = parseType()
    val methodName = consume(TokenType.IDENTIFIER, "Expect method name.")

    consume(TokenType.LEFT_PAREN, "Expect '(' after method name.")
    val args = ListBuffer[Argument]()
    if (!check(TokenType.RIGHT_PAREN)) {
      // NOTE: do-while looks a little weird, read link below
      // https://docs.scala-lang.org/scala3/reference/dropped-features/do-while.html
      while {
        val argType = parseType()
        val argName = consume(TokenType.IDENTIFIER, "Expect argument name.")
        args.addOne(Argument(argType, argName))

        isMatch(TokenType.COMMA)
      } do ()
    }

    consume(TokenType.RIGHT_PAREN, "Expect '(' after args.")

    consume(TokenType.LEFT_BRACE, "Expect '{' before method body")

    Stmt.Method(returnType, methodName, args.toList, Stmt.Block(block()))
  }

  private def parseType(): Type = {
    val typeName = consume(TokenType.IDENTIFIER, "Expect type for property.")
    if (isMatch(TokenType.LEFT_BRACKET)) {
      consume(TokenType.RIGHT_BRACKET, "Expect ']' after '['.")

      if (typeName.lexeme != "int") {
        throw error(typeName, "Only arrays of type `int` are allowed.")
      }

      Type.IntArray
    } else {
      typeName.lexeme match {
        case "int"     => Type.Int
        case "boolean" => Type.Bool
        case _         => Type.Custom(typeName.lexeme)
      }
    }
  }

  private def property(): Stmt.Property = {
    val propertyType = parseType()
    val name = consume(TokenType.IDENTIFIER, "Expect property name.")
    val assigned = if (isMatch(TokenType.SEMICOLON)) {
      None
    } else {
      consume(TokenType.EQUAL, "Missing ';' or '=' after property.")
      val expr = expression()
      consume(TokenType.SEMICOLON, "Expect ';' after expression.")
      Some(expr)
    }
    Stmt.Property(propertyType, name, assigned)
  }

  private def classDefinition(): Stmt.Class = {
    val name = consume(TokenType.IDENTIFIER, "Expect name for class.")

    // Check if class extends another
    val parentClass = if (isMatch(TokenType.EXTENDS)) {
      Some(
        Expr.Variable(
          consume(TokenType.IDENTIFIER, "Expect parent class name.")
        )
      )
    } else {
      None
    }

    consume(TokenType.LEFT_BRACE, "Expect '{' after name.")

    // start collecting methods and properties
    val methods = ListBuffer[Stmt.Method]()
    val properties = ListBuffer[Stmt.Property]()

    while (peek().typ != TokenType.RIGHT_BRACE) {
      // NOTE: methods always start with a `public` keyword, whereas properties don't
      // Start munching properties and methods!
      if (isMatch(TokenType.PUBLIC)) { // found a method!
        methods.addOne(method())
      } else { // Found a property
        properties.addOne(property())
      }
    }

    consume(TokenType.RIGHT_BRACE, "Expect '}' after class body.")

    Stmt.Class(name, parentClass, methods.toList, properties.toList)
  }

  def ifStatement(): Stmt = {
    val keyword = previous()
    consume(TokenType.LEFT_PAREN, "Expect '(' after if.")
    val condition = expression()
    consume(TokenType.RIGHT_PAREN, "Expect ')' after condition.")

    val ifBody = statement()

    // Else blocks are optional!
    val elseBody = if (isMatch(TokenType.ELSE)) {
      Some(statement())
    } else {
      None
    }

    Stmt.If(keyword, condition, ifBody, elseBody)
  }

  def whileStatement(): Stmt = {
    val keyword = previous()
    consume(TokenType.LEFT_PAREN, "Expect '(' after while.")
    val condition = expression()
    consume(TokenType.RIGHT_PAREN, "Expect ')' after condition.")
    val body = statement()

    Stmt.While(keyword, condition, body)
  }

  def printStatement(): Stmt = {
    val start = consume(TokenType.LEFT_PAREN, "Expect '(' after print.")
    val expr = expression()
    consume(TokenType.RIGHT_PAREN, "Expect ')' after print.")
    Stmt.Print(start, expr)
  }

  def assignStatement(): Stmt = {
    val nameOrType = previous()

    if (nameOrType.lexeme == "System") {
      consume(TokenType.PERIOD, "Expect '.' after 'System'.")
      val out = consume(TokenType.IDENTIFIER, "Expect 'out' after 'System.'.")
      if (out.lexeme != "out") {
        throw error(peek(), "Unexpected token after '.'.")
      }

      consume(TokenType.PERIOD, "Expect '.' after 'System.out'.")
      val println = consume(TokenType.IDENTIFIER, "Expect 'println' after 'System.out.'.")
      if (println.lexeme != "println") {
        throw error(peek(), "Unexpected token after '.'.")
      }

      consume(TokenType.LEFT_PAREN, "Expect '(' after print.")
      val toPrint = expression()
      consume(TokenType.RIGHT_PAREN, "Expect ')' after expression.")
      consume(TokenType.SEMICOLON, "Expect ';' after statement.")

      return Stmt.Print(nameOrType, toPrint)
    }

    if (isMatch(TokenType.EQUAL)) {
      val expr = expression()
      consume(TokenType.SEMICOLON, "Expect ';' after expression.")
      return Stmt.Assign(nameOrType, expr)
    }

    if (isMatch(TokenType.LEFT_BRACKET)) {
      // FIXME: hack around parsing an index set OR var decl of type array

      if (peek().typ == TokenType.RIGHT_BRACE) {
        if (nameOrType.lexeme != "int") {
          throw error(nameOrType, "Only arrays of type `int` are allowed.")
        }

        val name = consume(TokenType.IDENTIFIER, "Expected name for variable")
        return Stmt.Property(Type.IntArray, name)
      }

      val index = expression()
      consume(TokenType.RIGHT_BRACKET, "Expect ']' after index.")
      consume(TokenType.EQUAL, "Expect '=' after array index.")
      val value = expression()
      consume(TokenType.SEMICOLON, "Expect ';' after expression.")

      return Stmt.IndexSet(nameOrType, index, value)
    }

    if (isMatch(TokenType.IDENTIFIER)) {
      val name = previous()
      consume(TokenType.SEMICOLON, "Expect ';' after declaration.")

      return Stmt.Property(Type.fromString(nameOrType.lexeme), name)
    }

    throw error(peek(), "Not a statement.")
  }

  private def returnStatement(): Stmt = {
    val keyword = previous()
    val expr = expression()
    consume(TokenType.SEMICOLON, "Expect ';' after expression.")
    Stmt.Return(keyword, expr)
  }

  private def statement(): Stmt = {
    if (isMatch(TokenType.IF)) {
      return ifStatement()
    }

    if (isMatch(TokenType.WHILE)) {
      return whileStatement()
    }

    if (isMatch(TokenType.PRINT)) {
      return printStatement()
    }

    if (isMatch(TokenType.RETURN)) {
      return returnStatement()
    }

    // 4 cases here: println, variable assign, array assign, or variable declaration (w/ optional assign)
    if (isMatch(TokenType.IDENTIFIER)) {
      return assignStatement()
    }

    if (isMatch(TokenType.LEFT_BRACE)) {
      return Stmt.Block(block())
    }

    throw error(peek(), "Expect statement.")
  }

  def parse(): Program | Null = {
    // If we cannot parse a main class, abort immediately
    val mainClass =
      try {
        mainClassDefinition()
      } catch {
        case _: ParseError => return null
      }

    val classes = ListBuffer[Stmt.Class]()
    while (!isAtEnd) {
      try {
        if (isMatch(TokenType.CLASS)) {
          classes.addOne(classDefinition())
        } else {
          synchronize()
        }
      } catch {
        case _: ParseError => synchronize()
      }
    }

    Program(mainClass, classes.toList)
  }
}
