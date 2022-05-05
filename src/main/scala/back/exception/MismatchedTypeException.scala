package com.github.nishi_7
package back.exception

import front.Type

class MismatchedTypeException(expected: Seq[Type], actual: Type) extends N2TCompilerException(
  s"Expected: ${expected.mkString(" or ")} / Actual: $actual"
) {
  def this(expected: Type, actual: Type) = this(Seq(expected), actual)
}

class NotClassRefException(actual: Type) extends N2TCompilerException(
  s"""The type $actual is not class reference"""
)
