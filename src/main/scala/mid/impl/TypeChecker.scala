package com.github.nishi_7
package mid.impl

import CompileContext.ClassContext
import back.RawASTNode.Location.{Argument, ArrayWithIndex, Constant, ThisPointer}
import back.RawASTNode.Statement._
import back.RawASTNode._
import back.SymbolTable.LocalVar
import back.exception.VMWriter.Command
import back.exception._
import back.{RawASTNode, SymbolTable}
import front.ParserValue.{Binary, ConstantExpression, ReferenceExpression, Unary}
import front.Type.{Any, ClassRef}
import front.{HeaderValue, ParserValue, Type}
import mid.PipelineProcess
import mid.impl.TypeChecker.TypeCheckTask.{assertAccessibleWithIndex, assertType, assertValueComparable}
import mid.util.SymbolTableMixin

import scala.collection.mutable

object TypeChecker extends PipelineProcess[ParserValue.Class, Seq[Subroutine]] {
  override def process(context: CompileContext[ParserValue.Class]): CompileContext[Seq[RawASTNode.Subroutine]] =
    new TypeCheckTask(context).execute()

  private class TypeCheckTask(private val context: CompileContext[ParserValue.Class]) {
    private val defCache = mutable.Map.empty[String, Map[String, SubroutineDef]]
    private val result = Map.newBuilder[String, ClassContext[Seq[Subroutine]]]
    private type ClassHeader = Map[String, SubroutineDef]

    def execute(): CompileContext[Seq[RawASTNode.Subroutine]] = {
      for ((clsName, _) <- context.classes) { getClassHeader(clsName, List(clsName)) }

      CompileContext(result.result(), context.headers)
    }

    private def getClassHeader(clsName: String, dependencyPath: List[String]): ClassHeader  = defCache.get(clsName) match {
      case Some(value) => value
      case None => {
        val srDef = context.classes.get(clsName) match {
          case Some(clsCtx) => {
            val clsDef = clsCtx.definition
            val thisPath = clsName :: dependencyPath
            val dependencies = clsDef.imports.classes.map { id =>
              if (thisPath.contains(id.name)) throw new CircularImportException(clsName, thisPath)
              id.name -> getClassHeader(id.name, thisPath)
            }.toMap + (clsName -> clsDef.subroutines.map { sr => sr.name -> toSubroutineDef(sr) }.toMap)
            val transformed = new TypeCheckForClass(clsName, clsDef, dependencies).execute()
            result.addOne(clsName, new ClassContext[Seq[RawASTNode.Subroutine]] {
              override val definition: Seq[Subroutine] = transformed
              override val writer: () => VMWriter = clsCtx.writer
            })

            val srDefs = Map.newBuilder[String, SubroutineDef]
            clsDef.subroutines.foreach { sr =>
              srDefs.addOne(sr.name, toSubroutineDef(sr))
            }
            srDefs.result()
          }
          case None =>
            context.headers.get(clsName).map { hf =>
              val dependencies = hf.imports.map { _.name }.toSet
              val thisPath = clsName :: dependencyPath
              dependencies.foreach { getClassHeader(_, thisPath) }
              new TypeCheckForHeader(hf.classDef, dependencies).execute()
            }.getOrElse { throw new UndefinedClassException(clsName) }
        }

        defCache(clsName) = srDef
        srDef
      }
    }

    private class TypeCheckForClass(
      private val className: String,
      private val clazz: ParserValue.Class,
      private val dependencies: Map[String, Map[String, SubroutineDef]]
    ) extends SymbolTableMixin {
      private val classMemSize = clazz.classVars.size
      // TODO: make the inner class
      private var currentSubroutineName: String = _
      private var currentSubroutineReturnType: Option[Type] = None

      def execute(): Seq[Subroutine] = {
        clazz.classVars.foreach(check)
        clazz.subroutines.map(check)
      }

      private def check(cv: ParserValue.ClassVar): Unit = cv match { case ParserValue.ClassVar(_, typ, name) =>
        symTable(name).foreach {
          case _: SymbolTable.ClassVar => throw new ClassVarDefinedTwiceException(className, cv.name)
        }

        assertClassExists(typ)
        symTable.define(cv)
      }

      private def check: ParserValue.Subroutine => Subroutine = { case ParserValue.Subroutine(cat, retType, name, params, body) =>
        currentSubroutineName = name
        currentSubroutineReturnType = retType.map { _.value }
        val headStmt = cat match {
          case ParserValue.Subroutine.Constructor => {
            symTable.startStaticSubroutine()
            Seq(
              RawASTNode.Statement.Assignment(
                ThisPointer,
                SubroutineCall("Sys", "alloc", Seq(LocationValue(Constant(classMemSize))))
              )
            )
          }
          case ParserValue.Subroutine.Function => {
            symTable.startMethod()
            Seq.empty
          }
          case ParserValue.Subroutine.Method => {
            symTable.startStaticSubroutine()
            Seq(Statement.Assignment(ThisPointer, LocationValue(Argument(0))))
          }
        }

        retType.foreach { t => assertClassExists(t.value) }
        params.foreach(check)
        body.vars.foreach(check)

        Subroutine(className, name, body.vars.size, headStmt ++ check(body.statements))
      }

      private def check(p: ParserValue.Subroutine.Parameter): Unit =
        p match { case ParserValue.Subroutine.Parameter(typ, name) =>
          symTable(name).foreach {
            case _: SymbolTable.Argument => throw new ParameterDefinedTwiceException(className, currentSubroutineName, name)
          }

          assertClassExists(typ)
          symTable.define(p)
        }

      private def check(lv: ParserValue.Var): Unit = lv match { case ParserValue.Var(typ, name) =>
        symTable(name).foreach {
          case _: LocalVar => throw new LocalVarDefinedTwiceException(className, currentSubroutineName, name)
        }

        assertClassExists(typ)
        symTable.define(lv)
      }

      private def check(stmts: Seq[ParserValue.Statement]): Seq[Statement] = stmts.map {
        case ParserValue.Statement.Let(name, index, valueExpr) => {
          val varInfo = symTable(name).getOrElse {
            throw new UndefinedVariableException(className, currentSubroutineName, name)
          }

          index match {
            case Some(indexExpr) => {
              assertAccessibleWithIndex(varInfo.typ)
              val indexInfo = check(indexExpr)
              if (indexInfo.typ != Type.Int) throw new MismatchedTypeException(ClassRef("Int"), indexInfo.typ)
              Assignment(ArrayWithIndex(symTableVarToLocation(varInfo), indexInfo.expr), check(valueExpr).expr)
            }
            case None => {
              val valueInfo = check(valueExpr)
              assertType(varInfo.typ, valueInfo)
              Assignment(symTableVarToLocation(varInfo), valueInfo.expr)
            }
          }
        }
        case ParserValue.Statement.If(cond, ifTrue, ifFalse) => {
          val condInfo = check(cond)
          assertType(Type.Boolean, condInfo)
          If(condInfo.expr, check(ifTrue), check(ifFalse))
        }
        case ParserValue.Statement.While(cond, body) => {
          val condInfo = check(cond)
          assertType(Type.Boolean, condInfo)
          While(condInfo.expr, check(body))
        }
        case ParserValue.Statement.Do(call) => Do(checkSubroutineCall(call).expr)
        case ParserValue.Statement.Return(expr) => currentSubroutineReturnType match {
          case Some(retType) => expr match {
            case Some(actRetExpr) => {
              val actRetInfo = check(actRetExpr)
              assertType(retType, actRetInfo)
              Return(Some(actRetInfo.expr))
            }
            case None => throw new ReturnNothingException(className, currentSubroutineName, retType)
          }
          case None => {
            if (expr.isDefined) throw new ReturnSomethingException(className, currentSubroutineName)
            Return(None)
          }
        }
      }

      private def check(expr: ParserValue.Expression) :EvaluatedExpression = expr match {
        case Unary.Inverse(rator) => {
          val info = check(rator)
          assertType(Type.Int, info)
          EvaluatedExpression(Type.Int, CommandCall(Command.Neg, Seq(info.expr)))
        }
        case Unary.BitFlip(rator) => {
          val info = check(rator)
          assertType(Seq(Type.Int, Type.Boolean), info)
          EvaluatedExpression(info.typ, CommandCall(Command.Not, Seq(info.expr)))
        }

        case Binary.Addition(lhs, rhs) => checkBinary(Command.Add, lhs, rhs, Type.Int)
        case Binary.Subtract(lhs, rhs) => checkBinary(Command.Sub, lhs, rhs, Type.Int)
        case Binary.Multiplication(lhs, rhs) =>
          checkSubroutineCall("Math", "multiply", BuiltinFunctions.Multiply, Seq(lhs, rhs))
        case Binary.Division(lhs, rhs) =>
          checkSubroutineCall("Math", "divide", BuiltinFunctions.Divide, Seq(lhs, rhs))
        case Binary.Gt(lhs, rhs) => checkBinary(Command.Gt, lhs, rhs, Type.Int)
        case Binary.Lt(lhs, rhs) => checkBinary(Command.Lt, lhs, rhs, Type.Int)
        case Binary.Eq(lhs, rhs) => {
          val lhsInfo = check(lhs)
          val rhsInfo = check(rhs)
          assertValueComparable(lhsInfo.typ, rhsInfo.typ)
          EvaluatedExpression(Type.Boolean, CommandCall(Command.Eq, Seq(lhsInfo.expr, rhsInfo.expr)))
        }
        case Binary.And(lhs, rhs) => checkBinary(Command.And, lhs, rhs, Type.Boolean)
        case Binary.Or(lhs, rhs) => checkBinary(Command.Or, lhs, rhs, Type.Boolean)

        case ReferenceExpression.VarRef(name) => {
          val info = lookupFromSymTable(name)
          EvaluatedExpression(info.typ, LocationValue(symTableVarToLocation(info)))
        }
        case ReferenceExpression.ArrayAccess(name, indexExp) => {
          val varInfo = lookupFromSymTable(name)
          assertAccessibleWithIndex(varInfo.typ)
          val indexInfo = check(indexExp)
          assertType(Type.Int, indexInfo)
          EvaluatedExpression(Type.Any, LocationValue(ArrayWithIndex(symTableVarToLocation(varInfo), indexInfo.expr)))
        }
        case call: ReferenceExpression.SubroutineCall => checkSubroutineCall(call)

        case ConstantExpression.True => EvaluatedExpression(Type.Boolean, LocationValue(Constant(1)))
        case ConstantExpression.False => EvaluatedExpression(Type.Boolean, LocationValue(Constant(0)))
        case ConstantExpression.Null => EvaluatedExpression(Type.Null, LocationValue(Constant(0)))
        case ConstantExpression.This => EvaluatedExpression(Type.ClassRef(className), LocationValue(ThisPointer))
        case ConstantExpression.Integer(int) => EvaluatedExpression(Type.Int, LocationValue(Constant(int)))
        case ParserValue.ConstantExpression.Literal(str) => EvaluatedExpression(StringRef, RawASTNode.Literal(str))
      }

      private def checkBinary(command: Command, lhs: ParserValue.Expression, rhs: ParserValue.Expression, expectedType: Type) = {
        val lhsInfo = check(lhs)
        assertType(expectedType, lhsInfo)
        val rhsInfo = check(rhs)
        assertType(expectedType, rhsInfo)
        EvaluatedExpression(expectedType, CommandCall(command, Seq(lhsInfo.expr, rhsInfo.expr)))
      }

      private def checkSubroutineCall(call: ReferenceExpression.SubroutineCall): EvaluatedExpressionBase[SubroutineCall] =
        call.receiver match {
          case Some(receiver) => symTable(receiver) match {
            case Some(varInfo) => varInfo.typ match {
              case ClassRef(typName) => {
                val srDef = dependencies.getOrElse(typName, { throw new RuntimeException("The compiler is crashed due to a bug") })
                  .getOrElse(call.name, { throw new UndefinedSubroutineException(typName, call.name) })
                checkSubroutineCall(typName, call.name, srDef, call.args)
              }
              case _ => throw new N2TCompilerException(s"The class ${varInfo.typ} has no subroutine")
            }
            case None => {
              val srDef = dependencies.getOrElse(receiver, { throw new UndefinedClassException(receiver) })
                .getOrElse(call.name, { throw new UndefinedSubroutineException(receiver, call.name) })
              checkSubroutineCall(receiver, call.name, srDef, call.args)
            }
          }
          case None => dependencies(className).get(call.name) match {
            case Some(srDef) => checkSubroutineCall(className, call.name, srDef, call.args)
            case None => throw new UndefinedSubroutineException(className, call.name)
          }
        }

      private def checkSubroutineCall(
        className: String, srName: String, srDef: SubroutineDef, args: Seq[ParserValue.Expression]
      ): EvaluatedExpressionBase[SubroutineCall] = {
        if (srDef.args.length != args.length) throw new ArgumentLengthNotMatchedException()
        val argExprs = args.map(check)
          .zip(srDef.args)
          .map { case (expr, typ) =>
            assertType(typ, expr)
            expr.expr
          }

        EvaluatedExpressionBase(srDef.returnType.getOrElse(Type.Void), SubroutineCall(className, srName, argExprs))
      }

      private def assertClassExists: Type => Unit = {
        case Type.ClassRef(name) =>
          if (!dependencies.contains(name)) throw new UndefinedClassException(name)
        case _ => Unit
      }

      private def lookupFromSymTable(name: String) =
        symTable(name).getOrElse { throw new UndefinedVariableException(className, currentSubroutineName, name) }
    }

    private class TypeCheckForHeader(
      clazz: HeaderValue.Class,
      dependencies: Set[String]
    ) {
      def execute(): ClassHeader = clazz.subroutines.map { sr =>
        sr.returnType.foreach { t => assertClassExists(t.value) }
        sr.params.foreach { p => assertClassExists(p.typ.value) }
        sr.name -> toSubroutineDef(sr)
      }.toMap

      private def assertClassExists: Type => Unit = {
        case ClassRef(name) =>
          if (!dependencies.contains(name)) throw new UndefinedClassException(name)
        case _ => Unit
      }
    }
  }

  object TypeCheckTask {
    private def assertValueComparable(t1: Type, t2: Type): Unit = if (!{
      if (t1 == Type.Void || t2 == Type.Void) false
      else if (t1 == Type.Any || t2 == Type.Any) true
      else if (t1.nullable) t2.nullable
      else t1 == t2
    }) throw new IllegalComparisonException(t1, t2)

    private def assertAccessibleWithIndex(typ: Type): Unit = typ match {
      case ClassRef("Array") | Any =>
      case _ => throw new MismatchedTypeException(ClassRef("Array"), typ)
    }

    private def assertType(expected: Type, actual: EvaluatedExpression): Unit = assertType(expected, actual.typ)

    private def assertType(expected: Type, actual: Type): Unit = {
      if (expected != actual && actual != Any && !(actual == Type.Null && expected.nullable))
        throw new MismatchedTypeException(expected, actual)
    }

    private def assertType(expected: Seq[Type], actual: Type): Unit = {
      if (expected.contains(actual) && actual != Any && !(actual == Type.Null && expected.exists { _.nullable }))
        throw new MismatchedTypeException(expected, actual)
    }

    private def assertType(expected: Seq[Type], actual: EvaluatedExpression): Unit = assertType(expected, actual.typ)
  }

  private def toSubroutineDef(sr: ParserValue.Subroutine) =
    SubroutineDef(sr.params.map { _.typ }, sr.returnType.map { _.value })

  private def toSubroutineDef(sr: HeaderValue.Subroutine) =
    SubroutineDef(sr.params.map { _.typ }, sr.returnType.map { _.value })

  private case class SubroutineDef(args: Seq[Type], returnType: Option[Type])

  private case class EvaluatedExpressionBase[+Expr <: Expression](typ: Type, expr: Expr)

  private type EvaluatedExpression = EvaluatedExpressionBase[Expression]

  private def EvaluatedExpression(typ: Type, expr: Expression) = EvaluatedExpressionBase(typ, expr)

  private val StringRef = ClassRef("String")

  //noinspection TypeAnnotation
  private object BuiltinFunctions {
    val Multiply = SubroutineDef(Seq(Type.Int, Type.Int), Some(Type.Int))
    val Divide = SubroutineDef(Seq(Type.Int, Type.Int), Some(Type.Int))
  }

  import scala.language.implicitConversions

  implicit def unwrap(wrapped: ParserValue.TypeWrap): Type = wrapped.value
  implicit def unwrap(wrapped: HeaderValue.TypeWrap): Type = wrapped.value
}
