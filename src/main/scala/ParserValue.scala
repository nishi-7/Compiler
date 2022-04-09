package com.github.nishi_7

import ParserValue.ClassVar.VarType
import ParserValue.Subroutine.{Parameter, SubroutineCat}

sealed trait ParserValue

object ParserValue {
  sealed trait Node extends ParserValue

  case class Class(name: String, classVars: Seq[ClassVar], subroutines: Seq[Subroutine]) extends Node

  case class ClassVar(varType: VarType, typ: Type, name: String) extends Node

  object ClassVar {
    sealed trait VarType extends Node

    case object Static extends VarType
    case object Field extends VarType
  }

  sealed trait Type extends Node

  object Type {
    case object Int extends Type
    case object Char extends Type
    case object Boolean extends Type
    case class ClassRef(name: String) extends Type
  }

  case class Subroutine(category: SubroutineCat, returnType: scala.Option[Type], name: String, params: Seq[Parameter], body: Subroutine.Body) extends Node

  object Subroutine {
    sealed trait SubroutineCat extends Node

    case object Constructor extends SubroutineCat
    case object Function extends SubroutineCat
    case object Method extends SubroutineCat

    case class Parameter(typ: Type, name: String) extends Node

    case class Body(vars: Seq[Var], statements: Seq[Statement]) extends Node
  }

  case class Var(typ: Type, name: String) extends Node

  sealed trait Statement extends Node

  object Statement {
    case class Let(name: String, index: scala.Option[Expression], value: Expression) extends Statement

    case class If(cond: Expression, ifTrue: Seq[Statement], ifFalse: Seq[Statement]) extends Statement

    case class While(cond: Expression, body: Seq[Statement]) extends Statement

    case class Do(call: ReferenceExpression.SubroutineCall) extends Statement

    case class Return(expression: scala.Option[Expression]) extends Statement
  }

  sealed trait Expression extends Node

  sealed trait Unary extends Expression

  object Unary {
    case class Inverse(rator: Expression) extends Unary
    case class BitFlip(rator: Expression) extends Unary
  }

  sealed trait Binary extends Expression

  object Binary {
    case class Addition(lhs: Expression, rhs: Expression) extends Expression
    case class Subtract(lhs: Expression, rhs: Expression) extends Expression
    case class Multiplication(lhs: Expression, rhs: Expression) extends Expression
    case class Division(lhs: Expression, rhs: Expression) extends Expression
    case class Gt(lhs: Expression, rhs: Expression) extends Expression
    case class Lt(lhs: Expression, rhs: Expression) extends Expression
    case class Eq(lhs: Expression, rhs: Expression) extends Expression
    case class And(lhs: Expression, rhs: Expression) extends Expression
    case class Or(lhs: Expression, rhs: Expression) extends Expression
  }

  sealed trait ReferenceExpression extends Expression

  object ReferenceExpression {
    case class VarRef(name: String) extends ReferenceExpression
    case class ArrayAccess(name: String, index: Expression) extends ReferenceExpression
    case class SubroutineCall(receiver: scala.Option[String], name: String, args: Seq[Expression]) extends ReferenceExpression
  }

  sealed trait ConstantExpression extends Expression

  object ConstantExpression {
    case object True extends ConstantExpression
    case object False extends ConstantExpression
    case object Null extends ConstantExpression
    case object This extends ConstantExpression

    case class Integer(value: Int) extends ConstantExpression
    case class Literal(value: String) extends ConstantExpression
  }

  sealed trait Util extends ParserValue

  sealed trait List[+T <: ParserValue] extends Util with Seq[T] {
  }
  object List {
    case class Cons[+T <: ParserValue](hd: T, tl: List[T]) extends List[T] {
      override def length: Int = tail.length + 1

      override def iterator: Iterator[T] = Iterator(hd) ++ tl.iterator

      override def apply(i: Int): T = if (i == 0) hd else tl.apply(i - 1)
    }
    case object Empty extends List[Nothing] {
      override def length: Int = 0

      override def iterator: Iterator[Nothing] = Iterator.empty

      override def apply(i: Int): Nothing = throw new NoSuchElementException
    }
  }

  sealed trait Option[+T <: ParserValue] extends Util {
    def toOption: scala.Option[T]
  }
  object Option {
    case class Some[+T <: ParserValue](value: T) extends Option[T] {
      override def toOption: scala.Option[T] = scala.Some(value)
    }
    case object None extends Option[Nothing] {
      override def toOption: scala.Option[Nothing] = scala.None
    }

    def fromOption[T <: ParserValue](option: scala.Option[T]): Option[T] = option match {
      case scala.Some(value) => Some(value)
      case scala.None => None
    }
  }

  case class Identifier(name: String) extends Util
}
