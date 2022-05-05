package com.github.nishi_7

import CompileContext.ClassContext
import back.exception.VMWriter
import front.HeaderValue.HeaderFile

case class CompileContext[Node](classes: Map[String, ClassContext[Node]], headers: Map[String, HeaderFile])

object CompileContext {
  abstract class ClassContext[Node]() {
    val definition: Node

    val writer: () => VMWriter
  }
}
