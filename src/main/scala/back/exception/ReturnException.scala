package com.github.nishi_7
package back.exception

import front.Type

class ReturnNothingException(className: String, subroutineName: String, expectedType: Type) extends N2TCompilerException(
  s"""The subroutine $subroutineName in the class $className should return any $expectedType value"""
)

class ReturnSomethingException(className: String, subroutineName: String) extends N2TCompilerException(
  s"""The subroutine $subroutineName in the class $className cannot return any value"""
)
