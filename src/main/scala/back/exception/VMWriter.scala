package com.github.nishi_7
package back.exception

import back.exception.VMWriter.{Command, Segment}

trait VMWriter {
  def writePush(seg: Segment, index: Int): Unit

  def writePop(seg: Segment, index: Int): Unit

  def writeArithmetic(com: Command): Unit

  def writeLabel(name: String): Unit

  def writeGoto(name: String): Unit

  def writeIf(name: String): Unit

  def writeCall(className: String, srName: String, nArgs: Int): Unit

  def writeFunction(className: String, funcName: String, nLocals: Int): Unit

  def writeReturn(): Unit
}

object VMWriter {
  sealed abstract class Segment(val segName: String)
  object Segment {
    case object Const extends Segment("constant")
    case object Arg extends Segment("argument")
    case object Local extends Segment("local")
    case object Static extends Segment("static")
    case object This extends Segment("this")
    case object That extends Segment("that")
    case object Pointer extends Segment("pointer")
    case object Temp extends Segment("temp")
  }

  sealed abstract class Command(val comName: String)
  object Command {
    case object Add extends Command("add")
    case object Sub extends Command("sub")
    case object Neg extends Command("neg")
    case object Eq extends Command("eq")
    case object Gt extends Command("gt")
    case object Lt extends Command("lt")
    case object And extends Command("and")
    case object Or extends Command("or")
    case object Not extends Command("not")
  }
}
