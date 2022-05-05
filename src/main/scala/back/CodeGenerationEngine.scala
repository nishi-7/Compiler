package com.github.nishi_7
package back

import back.CodeGenerationEngine.Label
import back.RawASTNode._
import back.exception.VMWriter
import back.exception.VMWriter.{Command, Segment}

import java.nio.charset.StandardCharsets

private class CodeGenerationEngine(private val subroutines: Seq[Subroutine], private val writer: VMWriter) {
  def execute(): Unit = subroutines.foldLeft(Label.zero)(generate)

  private def generate(nextLabel: Label, sr: Subroutine): Label = {
    writer.writeFunction(sr.className, sr.subroutineName, sr.localVarSize)

    generate(nextLabel, sr.body)
  }

  private def generate(nextLabel: Label, block: Block): Label =
    block.foldLeft(nextLabel)(generate)

  private def generate(nextLabel: Label, stmt: Statement): Label = stmt match {
    case Statement.Assignment(assignTo, value) => {
      generate(value)
      popTo(assignTo)
      nextLabel
    }
    case Statement.If(cond, ifTrue, ifFalse) => {
      generate(cond)
      writer.writeIf(nextLabel)
      val ifTrueEndLabel = generate(nextLabel.inc, ifFalse)
      writer.writeGoto(ifTrueEndLabel)
      writer.writeLabel(nextLabel)
      val returnLabel = generate(ifTrueEndLabel.inc, ifTrue)
      writer.writeLabel(ifTrueEndLabel)
      returnLabel
    }
    case Statement.While(cond, body) => {
      val whileTopLabel = nextLabel
      val whileEndLabel = whileTopLabel.inc
      val whileBodyLabel = whileEndLabel.inc
      val newLabel = whileBodyLabel.inc
      writer.writeLabel(whileTopLabel)
      generate(cond)
      writer.writeIf(whileBodyLabel)
      writer.writeGoto(whileEndLabel)
      writer.writeLabel(whileBodyLabel)
      val returnLabel = generate(newLabel, body)
      writer.writeGoto(whileTopLabel)
      writer.writeLabel(whileEndLabel)
      returnLabel
    }
    case Statement.Do(call) => {
      generateSubroutineCall(call)
      writer.writePop(Segment.Temp, 0)
      nextLabel
    }
    case Statement.Return(exprOpt) => {
      exprOpt match {
        case Some(expr) => generate(expr)
        case None => writer.writePush(Segment.Const, 0)
      }
      writer.writeReturn()
      nextLabel
    }
  }

  private def generate(expr: Expression): Unit = expr match {
    case RawASTNode.LocationValue(location) => pushFrom(location)
    case RawASTNode.CommandCall(rator, rands) => {
      rands.foreach(generate)
      writer.writeArithmetic(rator)
    }
    case call: RawASTNode.SubroutineCall => generateSubroutineCall(call)
    case RawASTNode.Literal(str) => {
      val rawStr = str.getBytes(StandardCharsets.UTF_8)
      writer.writePush(Segment.Const, str.length)
      writer.writeCall("String", "new", 1)
      for (c <- rawStr) {
        if (c < 0x10000) {
          writer.writePush(Segment.Const, c)
          writer.writeCall("String", "appendChar", 2)
        } else {
          writer.writePush(Segment.Const, 0xffff - c)
          writer.writeArithmetic(Command.Not)
          writer.writeCall("String", "appendChar", 2)
        }
      }
    }
  }

  private def generateSubroutineCall(call: RawASTNode.SubroutineCall): Unit = {
    call.args.foreach(generate)
    writer.writeCall(call.className, call.subroutineName, call.args.size)
  }

  private def pushFrom(location: Location): Unit = location match {
    case Location.Constant(index) => writer.writePush(Segment.Const, index)
    case Location.Static(index) => writer.writePush(Segment.Static, index)
    case Location.Field(index) => writer.writePush(Segment.This, index)
    case Location.Argument(index) => writer.writePush(Segment.Arg, index)
    case Location.Local(index) => writer.writePush(Segment.Local, index)
    case lc: Location.ArrayWithIndex => {
      handleArrayWithIndex(lc)
      writer.writePush(Segment.That, 0)
    }
    case Location.ThisPointer => writer.writePush(Segment.Pointer, 0)
  }

  private def popTo(location: Location): Unit = location match {
    case Location.Constant(_) => throw new RuntimeException("It is prohibited to pop to the constant register")
    case Location.Static(index) => writer.writePop(Segment.Static, index)
    case Location.Field(index) => writer.writePop(Segment.This, index)
    case Location.Argument(index) => writer.writePop(Segment.Arg, index)
    case Location.Local(index) => writer.writePop(Segment.Local, index)
    case lc: Location.ArrayWithIndex => {
      handleArrayWithIndex(lc)
      writer.writePop(Segment.That, 0)
    }
    case Location.ThisPointer => writer.writePop(Segment.Pointer, 0)
  }

  private def handleArrayWithIndex(lc: Location.ArrayWithIndex): Unit = {
    pushFrom(lc.head)
    generate(lc.index)
    writer.writeArithmetic(Command.Add)
    writer.writePop(Segment.Pointer, 1)
  }
}

object CodeGenerationEngine {
  private[CodeGenerationEngine] implicit class Label(private val value: Int) extends AnyVal {
    def inc: Label = Label(value + 1)

    def labelName: String = s"LABEL_$value"
  }

  private[CodeGenerationEngine] object Label {
    def zero: Label = Label(0)

    import scala.language.implicitConversions

    implicit def labelToString(label: Label): String = label.labelName
  }
}
