package com.github.nishi_7
package back.exception

class ClassVarDefinedTwiceException(className: String, varName: String) extends N2TCompilerException(
  s"""The class variable "$varName" is defined twice in the class $className"""
)

class SubroutineDefinedTwiceException(className: String, subroutineName: String) extends N2TCompilerException(
  s"""The subroutine "$subroutineName" is defined twice in the class $className"""
)

class ParameterDefinedTwiceException(className: String, subroutineName: String, paramName: String) extends N2TCompilerException(
  s"""The parameter "$paramName" is defined twice in the subroutine $subroutineName of the class $className"""
)

class LocalVarDefinedTwiceException(className: String, subroutineName: String, varName: String) extends N2TCompilerException(
  s"""The local variable "$varName" is defined twice in the subroutine $subroutineName of the class $className"""
)
