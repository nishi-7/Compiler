package com.github.nishi_7
package backend

import backend.VMWriter.{Command, Segment}

import java.io.{Flushable, Writer}

class VMWriter(private val writer: Writer) extends AutoCloseable with Flushable {
  def writePush(seg: Segment, index: Int): Unit = {
    writer.write(s"\tpush ${seg.segName} ${index}\n")
  }

  def writePop(seg: Segment, index: Int): Unit = {
    writer.write(s"\tpop ${seg.segName} ${index} \n")
  }

  def writeArithmetic(com: Command): Unit = {
    writer.write(s"\t${com.comName}\n")
  }

  def writeLabel(name: String): Unit = {
    writer.write(s"label $name\n")
  }

  def writeGoto(name: String): Unit = {
    writer.write(s"\tgoto $name\n")
  }

  def writeIf(name: String): Unit = {
    writer.write(s"\tif-goto $name\n")
  }

  def writeCall(className: String, srName: String, nArgs: Int): Unit = {
    writer.write(s"\tcall $className.$srName $nArgs\n")
  }

  def writeFunction(className: String, funcName: String, nLocals: Int): Unit = {
    writer.write(s"function $className.$funcName $nLocals\n")
  }

  def writeReturn(): Unit = {
    writer.write("\treturn\n")
  }

  override def close(): Unit = writer.close()

  override def flush(): Unit = writer.flush()
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
