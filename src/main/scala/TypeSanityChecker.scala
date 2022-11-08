package minijava

import minijava.visitor.TypeInfo

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

  def checkTypeTable: Unit = {
    checkForDuplicateMethods
  }
}
