package com.github.nishi_7
package mid.impl

import back.RawASTNode
import back.RawASTNode.Location._
import back.RawASTNode._
import back.exception.VMWriter.Command
import back.exception.{N2TCompilerException, UndefinedSubroutineException, UndefinedVariableException, VMWriter}
import front.ParserValue.Binary._
import front.ParserValue.ConstantExpression._
import front.ParserValue.ReferenceExpression.{ArrayAccess, SubroutineCall, VarRef}
import front.ParserValue.Unary.{BitFlip, Inverse}
import front.ParserValue.{Expression, Statement, Subroutine}
import front.{ParserValue, Type}
import mid.PipelineProcess
import mid.util.SymbolTableMixin

object NameResolver extends PipelineProcess[ParserValue.Class, Seq[RawASTNode.Subroutine]] {
  override def process(context: CompileContext[ParserValue.Class]): CompileContext[Seq[RawASTNode.Subroutine]] = {
    val classes = context.classes.map { case (name: String, context: CompileContext.ClassContext[ParserValue.Class]) =>
      name -> new CompileContext.ClassContext[Seq[RawASTNode.Subroutine]] {
        override val definition: Seq[RawASTNode.Subroutine] = processClass(context.definition)

        override val writer: () => VMWriter = context.writer
      }
    }

    CompileContext[Seq[RawASTNode.Subroutine]](classes, context.headers)
  }

  private def processClass(cls: ParserValue.Class): Seq[RawASTNode.Subroutine] =
    new NameResolverForClass(cls).convert()

  private class NameResolverForClass(private val classDef: ParserValue.Class) extends SymbolTableMixin {
    private var currentSubroutineName: String = _
    private val srSigs = classDef.subroutines.map { sr => sr.name -> sr }.toMap

    def convert(): Seq[RawASTNode.Subroutine] = {
      classDef.classVars.foreach(symTable.define)

      classDef.subroutines.map(fromSubroutine)
    }

    private def fromSubroutine(sr: Subroutine): RawASTNode.Subroutine = {
      currentSubroutineName = sr.name
      val headStmt = sr.category match {
        case Subroutine.Constructor => {
          symTable.startStaticSubroutine()
          Seq(
            RawASTNode.Statement.Assignment(
              ThisPointer,
              RawASTNode.SubroutineCall("Sys", "alloc", Seq(LocationValue(Constant(classMemSize))))
            )
          )
        }
        case Subroutine.Function => {
          symTable.startMethod()
          Seq.empty
        }
        case Subroutine.Method => {
          symTable.startStaticSubroutine()
          Seq(RawASTNode.Statement.Assignment(ThisPointer, LocationValue(Argument(0))))
        }
      }

      sr.params.foreach(symTable.define)
      sr.body.vars.foreach(symTable.define)

      RawASTNode.Subroutine(classDef.name, sr.name, sr.body.vars.size, headStmt ++ fromStatements(sr.body.statements))
    }

    private def fromStatements(stmts: Seq[ParserValue.Statement]): Seq[RawASTNode.Statement] = stmts.map {
      case Statement.Let(name, index, value) =>
        RawASTNode.Statement.Assignment(
          index match {
            case Some(indexExp) =>
              ArrayWithIndex(lookupLocationFromSymTable(name), fromExpression(indexExp))
            case None => lookupLocationFromSymTable(name)
          },
          fromExpression(value)
        )
      case Statement.If(cond, ifTrue, ifFalse) =>
        RawASTNode.Statement.If(fromExpression(cond), fromStatements(ifTrue), fromStatements(ifFalse))
      case Statement.While(cond, body) =>
        RawASTNode.Statement.While(fromExpression(cond), fromStatements(body))
      case Statement.Do(call) =>
        RawASTNode.Statement.Do(fromSubroutineCall(call))
      case Statement.Return(expression) =>
        RawASTNode.Statement.Return(expression.map(fromExpression))
    }

    private def fromExpression(expr: Expression): RawASTNode.Expression = expr match {
      case Inverse(rator) => CommandCall(Command.Neg, Seq(fromExpression(rator)))
      case BitFlip(rator) => CommandCall(Command.Not, Seq(fromExpression(rator)))

      case Addition(lhs, rhs) => CommandCall(Command.Add, Seq(fromExpression(lhs), fromExpression(rhs)))
      case Subtract(lhs, rhs) => CommandCall(Command.Sub, Seq(fromExpression(lhs), fromExpression(rhs)))
      case Multiplication(lhs, rhs) => RawASTNode.SubroutineCall("Math", "multiply", Seq(fromExpression(lhs), fromExpression(rhs)))
      case Division(lhs, rhs) => RawASTNode.SubroutineCall("Math", "divide", Seq(fromExpression(lhs), fromExpression(rhs)))
      case Gt(lhs, rhs) => CommandCall(Command.Gt, Seq(fromExpression(lhs), fromExpression(rhs)))
      case Lt(lhs, rhs) => CommandCall(Command.Lt, Seq(fromExpression(lhs), fromExpression(rhs)))
      case Eq(lhs, rhs) => CommandCall(Command.Eq, Seq(fromExpression(lhs), fromExpression(rhs)))
      case And(lhs, rhs) => CommandCall(Command.And, Seq(fromExpression(lhs), fromExpression(rhs)))
      case Or(lhs, rhs) => CommandCall(Command.Or, Seq(fromExpression(lhs), fromExpression(rhs)))

      case VarRef(name) => LocationValue(lookupLocationFromSymTable(name))
      case ArrayAccess(name, indexExp) => LocationValue(ArrayWithIndex(lookupLocationFromSymTable(name), fromExpression(indexExp)))
      case call: SubroutineCall => fromSubroutineCall(call)

      case True => LocationValue(Constant(1))
      case False => LocationValue(Constant(0))
      case Null => LocationValue(Constant(0))
      case This => LocationValue(ThisPointer)
      case Integer(int) => LocationValue(Constant(int))
      case ParserValue.ConstantExpression.Literal(str) => RawASTNode.Literal(str)
    }

    private def fromSubroutineCall(call: ParserValue.ReferenceExpression.SubroutineCall): RawASTNode.SubroutineCall = {
      val args = call.args.map(fromExpression)

      call.receiver match {
        case Some(receiver) => symTable(receiver) match {
          case Some(v) => {
            v.typ match {
              case Type.ClassRef(className) =>
                RawASTNode.SubroutineCall(className, call.name, Seq(LocationValue(symTableVarToLocation(v))) ++ args)
              case _ => throw new N2TCompilerException(s"The class ${v.typ} has no subroutine.")
            }
          }
          case None => RawASTNode.SubroutineCall(receiver, call.name, args)
        }
        case None => srSigs.get(call.name) match {
          case Some(sig) => {
            val actualArgs = if (sig.category == Subroutine.Method) Seq(LocationValue(ThisPointer)) ++ args else args
            RawASTNode.SubroutineCall(classDef.name, call.name, actualArgs)
          }
          case None => throw new UndefinedSubroutineException(classDef.name, call.name)
        }
      }
    }

    protected def lookupLocationFromSymTable(name: String): Location = symTable(name) match {
      case Some(value) => symTableVarToLocation(value)
      case None => throw new UndefinedVariableException(classDef.name, currentSubroutineName, name)
    }

    private val classMemSize: Int = classDef.classVars.size
  }
}
