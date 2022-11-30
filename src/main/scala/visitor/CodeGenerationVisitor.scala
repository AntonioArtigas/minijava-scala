package minijava
package visitor

import ast.Expr.Binary
import ast.Type.{Bool, Custom, IntArray}
import ast.*

import org.objectweb.asm.{ClassWriter, Label, MethodVisitor, Opcodes}

import java.io.FileOutputStream
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class CodeGenerationVisitor(val typeTable: Map[String, TypeInfo])
    extends Stmt.Visitor[Unit]
    with Expr.Visitor[Unit] {
  private val classWriters = ListBuffer[(String, ClassWriter)]() // class name, classwriter

  private val BYTECODE_VERSION = Opcodes.V1_8 // Java 8
  private val JAVA_OBJECT = "java/lang/Object"
  private val INIT = "<init>"

  private var currentClassVisitor: Option[ClassWriter] = None
  private var currentMethodVisitor: Option[MethodVisitor] =
    None // method writer, start, and end labels
  private var currentMethod: Option[Stmt.Method] = None

  private val variables = mutable.HashMap[String, (Int, Type)]()
  private var localVariableCount = 0

  private def defineConstructor(cw: ClassWriter, parent: String): Unit = {
    // constructor
    val constructorVisitor = cw.visitMethod(
      Opcodes.ACC_PUBLIC,
      INIT, // <init> means the constructor which is a method
      "()V", // Constructor with no arguments
      null,
      null
    )

    constructorVisitor.visitCode()
    constructorVisitor.visitIntInsn(Opcodes.ALOAD, 0)
    constructorVisitor.visitMethodInsn(
      Opcodes.INVOKESPECIAL,
      parent,
      INIT,
      "()V",
      false
    )
    constructorVisitor.visitInsn(Opcodes.RETURN)
    constructorVisitor.visitMaxs(1, 1)
    constructorVisitor.visitEnd()

  }

  // Statements

  override def visitMainClass(stmt: Stmt.MainClass): Unit = {
    val mainClassVisitor = ClassWriter(ClassWriter.COMPUTE_FRAMES)
    currentClassVisitor = Some(mainClassVisitor)

    // Create main class
    mainClassVisitor.visit(
      BYTECODE_VERSION,
      Opcodes.ACC_PUBLIC,
      stmt.name.lexeme,
      null, // generics signature
      JAVA_OBJECT, // parent class
      null // no interfaces
    )

    defineConstructor(mainClassVisitor, JAVA_OBJECT)

    stmt.body.accept(this)

    mainClassVisitor.visitEnd()

    currentClassVisitor = None

    classWriters.append((stmt.name.lexeme, mainClassVisitor))
  }

  override def visitClass(stmt: Stmt.Class): Unit = {
    val classWriter = ClassWriter(
      ClassWriter.COMPUTE_FRAMES
    )
    currentClassVisitor = Some(classWriter)

    val parentClass = stmt.parentClass.map(_.name.lexeme).getOrElse(JAVA_OBJECT)

    classWriter.visit(
      BYTECODE_VERSION,
      Opcodes.ACC_PUBLIC,
      stmt.name.lexeme,
      null,
      parentClass, // Parent class to inherit from
      null
    )

    defineConstructor(classWriter, parentClass)

    // write properties
    for (prop <- stmt.properties) {
      classWriter.visitField(
        Opcodes.ACC_PUBLIC,
        prop.name.lexeme,
        prop.typ.toTypeDescriptor,
        null,
        null
      )
    }

    stmt.methods.foreach(_.accept(this))

    classWriter.visitEnd()

    currentClassVisitor = None

    classWriters.append((stmt.name.lexeme, classWriter))
  }

  override def visitMainMethod(stmt: Stmt.MainMethod): Unit = {
    // create main method
    val mainMethodVisitor = currentClassVisitor.get.visitMethod(
      Opcodes.ACC_PUBLIC,
      "main",
      "([Ljava/lang/String;)V",
      "",
      Array()
    )

    currentMethodVisitor = Some(mainMethodVisitor)

    mainMethodVisitor.visitCode()

    stmt.body.accept(this)

    mainMethodVisitor.visitInsn(Opcodes.RETURN)
    mainMethodVisitor.visitMaxs(-1, -1) // NOTE: -1 means make asm calculate for us
    mainMethodVisitor.visitEnd()
  }

  override def visitBlock(stmt: Stmt.Block): Unit = stmt.statements.foreach(_.accept(this))

  override def visitMethod(stmt: Stmt.Method): Unit = {
    currentMethod = Some(stmt)

    val argTypes = stmt.arguments.map(_.typ.toTypeDescriptor)
    val returnType = stmt.returnType.toTypeDescriptor
    val typeSignature = s"(${argTypes.mkString(",")})$returnType"

    val methodVisitor = currentClassVisitor.get.visitMethod(
      Opcodes.ACC_PUBLIC,
      stmt.name.lexeme,
      typeSignature,
      null,
      null
    )

    currentMethodVisitor = Some(methodVisitor)

    methodVisitor.visitCode()

    localVariableCount = 0

    // Push args before hand
    for (arg <- stmt.arguments) {
      localVariableCount += 1

      variables.put(arg.name.lexeme, (localVariableCount, arg.typ))
    }

    stmt.body.accept(this)

    methodVisitor.visitEnd()

    variables.clear()
    currentMethod = None
  }

  // Must be inside method, local variables
  override def visitProperty(stmt: Stmt.Property): Unit = {
    val mv = currentMethodVisitor.get

    localVariableCount += 1

    // Push default value, then write the correct store op
    stmt.typ match
      case Type.Int =>
        mv.visitLdcInsn(Integer.valueOf(0))
        mv.visitVarInsn(Opcodes.ISTORE, localVariableCount)
      case Type.IntArray =>
        mv.visitInsn(Opcodes.ACONST_NULL)
        mv.visitVarInsn(Opcodes.ASTORE, localVariableCount)
      case Type.Bool =>
        mv.visitInsn(Opcodes.ICONST_0)
        mv.visitVarInsn(Opcodes.ISTORE, localVariableCount)
      case Type.Custom(_) =>
        mv.visitInsn(Opcodes.ACONST_NULL)
        mv.visitVarInsn(Opcodes.ASTORE, localVariableCount)

    // While inside a method, we need to keep track of what variables are pushed in the stack
    variables.put(stmt.name.lexeme, (localVariableCount, stmt.typ))
  }

  override def visitAssign(stmt: Stmt.Assign): Unit = {
    val mv = currentMethodVisitor.get

    val (index, typ) = variables(stmt.name.lexeme)

    // This will push a value on stack
    stmt.assigned.accept(this)

    typ match
      case Type.Int | Type.Bool => mv.visitVarInsn(Opcodes.ISTORE, index)
      case Type.IntArray        => mv.visitVarInsn(Opcodes.ASTORE, index)
      case Type.Custom(_)       => mv.visitVarInsn(Opcodes.ASTORE, index)
  }

  override def visitIf(stmt: Stmt.If): Unit = {
    val mv = currentMethodVisitor.get

    val elseLabel = Label()
    val endLabel = Label()

    // push condition value onto stack
    stmt.condition.accept(this)

    // handle if we need to emit code for optional else branch
    if (stmt.elseBranch.isDefined) {
      mv.visitJumpInsn(Opcodes.IFNE, elseLabel)
    }

    // Add then branch code
    stmt.thenBranch.accept(this)

    // Add else code afterwards if defined
    if (stmt.elseBranch.isDefined) {
      mv.visitJumpInsn(Opcodes.GOTO, endLabel)
      mv.visitLabel(elseLabel)
      stmt.elseBranch.get.accept(this)
    }

    mv.visitLabel(endLabel)
  }

  override def visitWhile(stmt: Stmt.While): Unit = {
    val mv = currentMethodVisitor.get

    val conditionLabel = new Label()
    val bodyLabel = new Label()

    // Label to jump back to condition
    mv.visitJumpInsn(Opcodes.GOTO, conditionLabel)

    // Body label and body
    mv.visitLabel(bodyLabel)
    stmt.body.accept(this)

    // Condition label and check
    mv.visitLabel(conditionLabel)
    stmt.condition.accept(this)
    mv.visitJumpInsn(Opcodes.IFEQ, bodyLabel)
  }

  override def visitPrint(stmt: Stmt.Print): Unit = {
    val mv = currentMethodVisitor.get

    // Load out object
    mv.visitFieldInsn(
      Opcodes.GETSTATIC,
      "java/lang/System",
      "out",
      "Ljava/io/PrintStream;"
    )

    // Load expr onto stack
    stmt.expression.accept(this)

    // Fire off the method
    mv.visitMethodInsn(
      Opcodes.INVOKEVIRTUAL,
      "java/io/PrintStream",
      "println",
      "(I)V", // Already checked if expression is NOT of type int
      false
    )
  }

  override def visitIndexSet(stmt: Stmt.IndexSet): Unit = {
    val mv = currentMethodVisitor.get

    val (index, _) = variables(stmt.obj.lexeme)

    mv.visitIntInsn(Opcodes.ALOAD, index)

    stmt.index.accept(this)

    stmt.value.accept(this)

    mv.visitInsn(Opcodes.IASTORE)
  }

  override def visitReturn(stmt: Stmt.Return): Unit = {
    val mv = currentMethodVisitor.get

    stmt.expr.accept(this)

    currentMethod.get.returnType match
      case Type.Int       => mv.visitInsn(Opcodes.IRETURN)
      case Type.IntArray  => mv.visitInsn(Opcodes.ARETURN)
      case Type.Bool      => mv.visitInsn(Opcodes.IRETURN)
      case Type.Custom(_) => mv.visitInsn(Opcodes.ARETURN)
  }

  // Expressions

  override def visitBinary(expr: Expr.Binary): Unit = {
    val mv = currentMethodVisitor.get

    expr.left.accept(this)
    expr.right.accept(this)
    expr.opKind match
      case BinaryOp.AND       => mv.visitInsn(Opcodes.IAND)
      case BinaryOp.DASH      => mv.visitInsn(Opcodes.ISUB)
      case BinaryOp.PLUS      => mv.visitInsn(Opcodes.IADD)
      case BinaryOp.MUL       => mv.visitInsn(Opcodes.IMUL)
      case BinaryOp.LESS_THAN =>
        // Workaround since JVM doesn't have an fcmp for ints which pushes -1, 0, 1 onto the stack.
        // Therefore we make our own icmp with IF_ICMPGE and pushing the relevant value onto the stack.
        val falseLabel = new Label()
        val endLabel = new Label()

        mv.visitJumpInsn(Opcodes.IF_ICMPGE, falseLabel)

        mv.visitInsn(Opcodes.ICONST_0)
        mv.visitJumpInsn(Opcodes.GOTO, endLabel)

        mv.visitLabel(falseLabel)
        mv.visitInsn(Opcodes.ICONST_1)

        mv.visitLabel(endLabel)
      case BinaryOp.GREATER_THAN =>
        throw UnsupportedOperationException("'>' operator not supported.")
      case BinaryOp.GREATER_EQUAL =>
        throw UnsupportedOperationException("'>=' operator not supported")
      case BinaryOp.LESS_EQUAL => throw UnsupportedOperationException("'<=' operator not supported")
  }

  override def visitIndexGet(expr: Expr.IndexGet): Unit = {
    val mv = currentMethodVisitor.get

    expr.obj.accept(this)
    expr.index.accept(this)

    mv.visitInsn(Opcodes.IALOAD)
  }

  override def visitGetLength(expr: Expr.GetLength): Unit = {
    val mv = currentMethodVisitor.get

    // push array onto stack
    expr.obj.accept(this)

    mv.visitInsn(Opcodes.ARRAYLENGTH)
  }

  // NOTE: In Java, ICONST_0 is true and ICONST_1 is false!
  override def visitBool(expr: Expr.Bool): Unit = {
    val mv = currentMethodVisitor.get
    val op = if (expr.value) {
      Opcodes.ICONST_0
    } else {
      Opcodes.ICONST_1
    }

    mv.visitInsn(op)
  }

  override def visitInteger(expr: Expr.Integer): Unit = {
    val mv = currentMethodVisitor.get

    mv.visitLdcInsn(expr.value)
  }

  override def visitGrouping(expr: Expr.Grouping): Unit = expr.inner.accept(this)

  override def visitThis(expr: Expr.This): Unit = {
    val mv = currentMethodVisitor.get

    mv.visitIntInsn(Opcodes.ALOAD, 0) // 0 refers to this on aload
  }

  override def visitCall(expr: Expr.Call): Unit = {
    val mv = currentMethodVisitor.get

    // push object on stack
    expr.calle.accept(this)

    // push args onto stack
    expr.args.foreach(_.accept(this))

    // NOTE: Use information added during type checking phase
    val argTypes = expr.method.get.meth.arguments.map(_.typ.toTypeDescriptor)
    val returnType = expr.method.get.meth.returnType.toTypeDescriptor

    // perform the call
    mv.visitMethodInsn(
      Opcodes.INVOKEVIRTUAL,
      expr.clsName.get,
      expr.name.lexeme,
      s"(${argTypes.mkString(",")})$returnType",
      false
    )
  }

  override def visitVariable(expr: Expr.Variable): Unit = {
    val mv = currentMethodVisitor.get

    val (index, typ) = variables(expr.name.lexeme)

    typ match
      case Type.Int | Type.Bool => mv.visitVarInsn(Opcodes.ILOAD, index)
      case Type.IntArray        => mv.visitVarInsn(Opcodes.ALOAD, index)
      case Type.Custom(_)       => mv.visitVarInsn(Opcodes.ALOAD, index)
  }

  override def visitNewArray(expr: Expr.NewArray): Unit = {
    val mv = currentMethodVisitor.get

    // push count
    expr.count.accept(this)

    mv.visitIntInsn(Opcodes.NEWARRAY, Opcodes.T_INT)
  }

  override def visitNewInstance(expr: Expr.NewInstance): Unit = {
    val mv = currentMethodVisitor.get

    mv.visitTypeInsn(Opcodes.NEW, expr.typ.lexeme)
    mv.visitInsn(Opcodes.DUP)

    mv.visitMethodInsn(
      Opcodes.INVOKESPECIAL,
      expr.typ.lexeme,
      "<init>",
      "()V",
      false
    )
  }

  override def visitUnary(expr: Expr.Unary): Unit = {
    val mv = currentMethodVisitor.get

    expr.left.accept(this)

    expr.opKind match
      case UnaryOp.BANG =>
        mv.visitInsn(Opcodes.ICONST_1)
        mv.visitInsn(Opcodes.IXOR)
      case UnaryOp.NEGATE =>
        mv.visitInsn(Opcodes.INEG)
  }

  def compileProgram(stmt: Program, outDir: String): Unit = {
    stmt.mainClass.accept(this)

    for (cls <- stmt.classes) {
      cls.accept(this)
    }

    for ((className, classWriter) <- classWriters) {
      val path = s"$outDir/$className.class"
      val output = FileOutputStream(path)
      try {
        output.write(classWriter.toByteArray)
        println(s"Wrote file $path")
      } finally {
        if (output != null) {
          output.close()
        }
      }
    }

    // TODO: Compile program into files
  }
}
