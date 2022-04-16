package com.github.nishi_7
package backend.exception

import front.ParserValue

class MismatchedTypeException(name: String, expected: ParserValue.Type, actual: ParserValue.Type) extends N2TCompilerException(
  s"Expected: $expected / Actual: $actual"
)
