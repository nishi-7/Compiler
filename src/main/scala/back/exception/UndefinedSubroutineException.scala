package com.github.nishi_7
package back.exception

class UndefinedSubroutineException(className: String, subroutineName: String) extends N2TCompilerException(
  s"The subroutine $subroutineName is undefined in the class $subroutineName."
)
