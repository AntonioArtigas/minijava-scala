package minijava

import minijava.visitor.TypeInfo
import minijava.visitor.MethodDeclaration

/** Simple checks on types to prevent edge cases. More complicated things like seeing if variables
  * use undeclared types is done in the TypeCheckerVisitor
  *
  * @param typeTable
  *   Type table to check through
  */
class TypeSanityChecker(val typeTable: Map[String, TypeInfo]) {
  def checkForDuplicateMethods: Unit = {
    for (typ <- typeTable.values) {
      typ match
        case cls: TypeInfo.ClassType =>
          cls.methods
            .groupBy(meth => (meth.name.lexeme, meth.args, meth.returnType))
            .filter(_._2.length > 1)
            .foreach(dup => {
              MiniJava.error(
                cls.name,
                s"Duplicate method ${dup._1._1} in class ${cls.name.lexeme}"
              )
            })
        case _ =>
    }
  }

  // Tuple of method and whether or not it's from the parent of the class
  def getAllMethods(cls: TypeInfo.ClassType): List[(MethodDeclaration, Boolean)] = {
    cls.parent match
      case None => cls.methods.map((_, false))
      case Some(value) =>
        cls.methods.map((_, false)) ++ getAllMethods(
          typeTable.get(cls.name.lexeme).get.asInstanceOf[TypeInfo.ClassType]
        ).map(p => (p._1, true))
  }

  def checkForBadMethodOverloading: Unit = {
    for (typ <- typeTable.values) {
      typ match
        case cls: TypeInfo.ClassType =>
          val classMethods = getAllMethods(cls)

          classMethods
            .groupBy(_._1.name.lexeme)
            .foreach(methGroup => {
              val name = methGroup._1
              val methods = methGroup._2

              methods
                .groupBy(_._1.args.map(_.typ))
                .foreach(pm => {
                  val parameterTypes = pm._1
                  val methodMatches = pm._2

                  val childMatches = methodMatches.filter(_._2 == false).map(_._1)
                  val parentMatches = methodMatches.filter(_._2 == true).map(_._1)

                  // Filter out the parent methods that have return types that are conformed to by the return types of
                  // child class methods
                  val parentNonCovariant = parentMatches.filter(p => {
                    childMatches.exists(c =>
                      typeTable
                        .get(c.returnType)
                        .get
                        .coerce(typeTable.get(p.returnType).get, typeTable)
                        .isEmpty
                    )
                  })

                  val allMatches = childMatches ++ parentNonCovariant
                  if (allMatches.distinct.length > 1) {
                    val declsAsStrings = allMatches
                      .sortBy(_.returnType)
                      .map(m =>
                        s"${m.returnType} ${m.name.lexeme}(${m.args.map(_.typ).mkString(",")})"
                      )

                    MiniJava.error(
                      cls.name,
                      s"Cannot overload methods with same parameters BUT different return types.\n"
                        + declsAsStrings.mkString("\n")
                    )
                  }
                })
            })

        case _ =>
    }
  }

  def checkTypeTable: Unit = {
    checkForDuplicateMethods

    if (MiniJava.hadError) {
      return
    }

    checkForBadMethodOverloading
  }
}
