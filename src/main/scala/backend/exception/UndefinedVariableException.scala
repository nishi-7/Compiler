package com.github.nishi_7
package backend.exception

class UndefinedVariableException(name: String) extends N2TCompilerException(
  s"Variable $name is not defined."
)
