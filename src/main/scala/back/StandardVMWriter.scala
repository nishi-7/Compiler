package com.github.nishi_7
package back

import back.exception.VMWriter
import back.exception.VMWriter.{Command, Segment}

import java.io.{Flushable, Writer}

class StandardVMWriter(private val writer: Writer) extends VMWriter with AutoCloseable
                                                                    with Flushable {
  def writePush(seg: Segment, index: Int): Unit = {
    writer.write(s"\tpush ${seg.segName} $index\n")
  }

  def writePop(seg: Segment, index: Int): Unit = {
    writer.write(s"\tpop ${seg.segName} $index \n")
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
