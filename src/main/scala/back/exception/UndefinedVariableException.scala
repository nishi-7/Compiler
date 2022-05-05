package com.github.nishi_7
package back.exception

class UndefinedVariableException(className: String, subroutineName: String, name: String) extends N2TCompilerException(
  s"The variable $name is undefined in the subroutine $subroutineName of the class $className."
)
