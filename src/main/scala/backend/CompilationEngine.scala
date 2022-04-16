package com.github.nishi_7
package backend

import backend.CompilationEngine.Label
import backend.SymbolTable.{Argument, FieldVar, LocalVar, StaticVar}
import backend.VMWriter.{Command, Segment}
import backend.exception.UndefinedVariableException
import front.ParserValue
import front.ParserValue.Binary._
import front.ParserValue.ConstantExpression._
import front.ParserValue.ReferenceExpression.{ArrayAccess, SubroutineCall, VarRef}
import front.ParserValue.Unary.{BitFlip, Inverse}
import front.ParserValue.{Statement, Subroutine, Type}

import java.io.Writer
import java.nio.charset.StandardCharsets

class CompilationEngine private(rawWriter: Writer) {
  private val writer = new VMWriter(rawWriter)

  private def execute(classDef: ParserValue.Class): Unit = new ClassWriter(classDef).startDefine()

  private class ClassWriter(private val classDef: ParserValue.Class) {
    private val symTable = new SymbolTable

    private val srSigs = classDef.subroutines.map { sr => sr.name -> sr }.toMap

    def startDefine(): Unit = {
      classDef.classVars.foreach(symTable.define)

      classDef.subroutines.foldLeft(Label.zero)(fromSubroutine)
    }

    private def fromSubroutine(label: Label, sr: ParserValue.Subroutine): Label = {
      writer.writeFunction(classDef.name, sr.name, sr.body.vars.size)

      sr.category match {
        case Subroutine.Constructor => {
          symTable.startStaticSubroutine()
          writer.writePush(Segment.Const, calcClassMemSize)
          writer.writeCall("Sys", "alloc", 1)
          writer.writePop(Segment.Pointer, 0)
        }
        case Subroutine.Method => {
          symTable.startMethod()
          writer.writePush(Segment.Arg, 0)
          writer.writePop(Segment.Pointer, 0)
        }
        case Subroutine.Function => {
          symTable.startStaticSubroutine()
        }
      }

      sr.params.foreach(symTable.define)
      sr.body.vars.foreach(symTable.define)

      fromStatements(label, sr.body.statements)
    }

    private def fromStatements(label: Label, stmts: Seq[ParserValue.Statement]): Label
    = stmts.foldLeft(label)(fromStatement)

    private def fromStatement(label: Label, stmt: ParserValue.Statement): Label = stmt match {
      case Statement.Let(name, index, exp) => {
        fromExpression(exp)
        index match {
          case Some(indexExp) => {
            fromExpression(indexExp)
            pushVariable(lookUpFromSymTable(name))
            writer.writeArithmetic(Command.Add)
            writer.writePop(Segment.Pointer, 1)
            writer.writePop(Segment.That, 0)
          }
          case None => {
            lookUpFromSymTable(name) match {
              case StaticVar(_, index) => writer.writePop(Segment.Static, index)
              case FieldVar(_, index) => writer.writePop(Segment.This, index)
              case Argument(_, index) => writer.writePop(Segment.Arg, index)
              case LocalVar(_, index) => writer.writePop(Segment.Local, index)
            }
          }
        }
        label
      }
      case Statement.If(cond, ifTrue, ifFalse) => {
        fromExpression(cond)
        writer.writeIf(label)
        val ifTrueEndLabel = fromStatements(label.inc, ifFalse)
        writer.writeGoto(ifTrueEndLabel)
        writer.writeLabel(label)
        val returnLabel = fromStatements(ifTrueEndLabel.inc, ifTrue)
        writer.writeLabel(ifTrueEndLabel)
        returnLabel
      }
      case Statement.While(cond, body) => {
        val whileTopLabel = label
        val whileEndLabel = whileTopLabel.inc
        val whileBodyLabel = whileEndLabel.inc
        val newLabel = whileBodyLabel.inc
        writer.writeLabel(whileTopLabel)
        fromExpression(cond)
        writer.writeIf(whileBodyLabel)
        writer.writeGoto(whileEndLabel)
        writer.writeLabel(whileBodyLabel)
        val returnLabel = fromStatements(newLabel, body)
        writer.writeGoto(whileTopLabel)
        writer.writeLabel(whileEndLabel)
        returnLabel
      }
      case Statement.Do(call) => {
        fromSubroutineCall(call)
        writer.writePop(Segment.Temp, 0) // TODO: pop unused return value
        label
      }
      case Statement.Return(expression) => {
        expression match {
          case Some(exp) => {
            fromExpression(exp)
            writer.writeReturn()
          }
          case None => {
            writer.writePush(Segment.Const, 0)
            writer.writeReturn()
          }
        }
        label
      }
    }

    private def fromExpression(exp: ParserValue.Expression): Unit = exp match {
      // Unary
      case Inverse(rator) => fromUnaryExpression(rator, Command.Neg)
      case BitFlip(rator) => fromUnaryExpression(rator, Command.Not)

      // Binary
      case Addition(lhs, rhs) => fromBinaryExpression(lhs, rhs, Command.Add)
      case Subtract(lhs, rhs) => fromBinaryExpression(lhs, rhs, Command.Sub)
      case Multiplication(lhs, rhs) => fromSubroutineCall(SubroutineCall(Some("Math"), "multiply", Seq(lhs, rhs)))
      case Division(lhs, rhs) => fromSubroutineCall(SubroutineCall(Some("Math"), "divide", Seq(lhs, rhs)))
      case Gt(lhs, rhs) => fromBinaryExpression(lhs, rhs, Command.Gt)
      case Lt(lhs, rhs) => fromBinaryExpression(lhs, rhs, Command.Lt)
      case Eq(lhs, rhs) => fromBinaryExpression(lhs, rhs, Command.Eq)
      case And(lhs, rhs) => fromBinaryExpression(lhs, rhs, Command.And)
      case Or(lhs, rhs) => fromBinaryExpression(lhs, rhs, Command.Or)

      // ReferenceExpression
      case VarRef(name) => pushVariable(lookUpFromSymTable(name))
      case ArrayAccess(name, indexExp) => {
        fromExpression(indexExp)
        pushVariable(lookUpFromSymTable(name))
        writer.writeArithmetic(Command.Add)
        writer.writePop(Segment.Pointer, 1)
        writer.writePush(Segment.That, 0)
      }
      case call: SubroutineCall => fromSubroutineCall(call)

      // ConstantExpression
      case True => {
        writer.writePush(Segment.Const, 1)
        writer.writeArithmetic(Command.Neg)
      }
      case False => writer.writePush(Segment.Const, 0)
      case Null => writer.writePush(Segment.Const, 0)
      case This => writer.writePush(Segment.Pointer, 0)
      case Integer(int) => writer.writePush(Segment.Const, int)
      case Literal(str) => {
        writer.writePush(Segment.Const, str.length)
        writer.writeCall("String", "new", 1)

        val rawStr = str.getBytes(StandardCharsets.UTF_8)
        for (c <- rawStr) {
          writer.writePush(Segment.Const, c)
          writer.writeCall("String", "appendChar", 2)
        }
      }
    }

    private def fromUnaryExpression(rator: ParserValue.Expression, cmd: Command): Unit = {
      fromExpression(rator)
      writer.writeArithmetic(cmd)
    }

    private def fromBinaryExpression(lhs: ParserValue.Expression, rhs: ParserValue.Expression, cmd: Command): Unit = {
      fromExpression(lhs)
      fromExpression(rhs)
      writer.writeArithmetic(cmd)
    }

    private def fromSubroutineCall(call: SubroutineCall): Unit = {
      var isStatic = true
      val className = call.receiver match {
        case Some(receiver) => symTable(receiver) match {
          case Some(v) => {
            pushVariable(v)
            isStatic = false
            v.typ match {
              case Type.ClassRef(name) => name
              case _ => throw new RuntimeException(s"Variable $receiver is not a instance of class.")
            }
          }
          case None => receiver
        }
        case None => {
          isStatic = srSigs(call.name).category != ParserValue.Subroutine.Method
          classDef.name
        }
      }
      for (arg <- call.args) {
        fromExpression(arg)
      }

      val argSize = call.args.size + (if (isStatic) 0 else 1)
      writer.writeCall(className, call.name, argSize)
    }

    private def calcClassMemSize: Int = classDef.classVars.size

    @inline
    private def lookUpFromSymTable(name: String): SymbolTable.Var = symTable(name) match {
      case Some(value) => value
      case None => throw new UndefinedVariableException(name)
    }
  }

  private def pushVariable(v: SymbolTable.Var): Unit = v match {
    case StaticVar(_, index) => writer.writePush(Segment.Static, index)
    case FieldVar(_, index) => writer.writePush(Segment.This, index)
    case Argument(_, index) => writer.writePush(Segment.Arg, index)
    case LocalVar(_, index) => writer.writePush(Segment.Local, index)
  }
}

object CompilationEngine {
  def write(classDef: ParserValue.Class, writer: Writer): Unit = new CompilationEngine(writer).execute(classDef)

  private implicit class Label(private val value: Int) extends AnyVal {
    def inc: Label = Label(value + 1)
  }

  private object Label {
    def zero: Label = Label(0)

    import scala.language.implicitConversions

    implicit def labelToString(label: Label): String = label.value.toString
  }
}
