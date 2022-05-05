package com.github.nishi_7
package back.exception

class UndefinedClassException(className: String) extends N2TCompilerException(
  s"""The class "$className" is not defined, but the program tried to use it"""
)
