package com.github.nishi_7
package backend.exception

import front.ParserValue

class NotInstanceException(name: String, actual: ParserValue.Type) extends N2TCompilerException(
  s"$name is not an instance of classes"
)