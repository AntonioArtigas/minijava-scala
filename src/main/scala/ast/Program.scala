package minijava
package ast

import ast.Stmt

case class Program(mainClass: Stmt.MainClass, classes: List[Stmt.Class])
