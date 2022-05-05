package com.github.nishi_7
package back.exception

class CircularImportException(className: String, dependencyPath: Seq[String]) extends N2TCompilerException(
  s"""The circular import is detected: $className<-${dependencyPath.mkString("<-")}"""
)
