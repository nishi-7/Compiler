package com.github.nishi_7
package back.exception

import front.Type

class NotInstanceException(name: String, actual: Type) extends N2TCompilerException(
  s"$name is not an instance of classes"
)
