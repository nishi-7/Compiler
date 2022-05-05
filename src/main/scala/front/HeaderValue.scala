package com.github.nishi_7
package front

import front.HeaderValue.Subroutine.Parameter

sealed trait HeaderValue

object HeaderValue {
  sealed trait HeaderNode extends HeaderValue

  case class HeaderFile(imports: Seq[Identifier], classDef: Class) extends HeaderValue

  case class Class(name: String, subroutines: Seq[Subroutine]) extends HeaderNode

  case class Subroutine(category: Subroutine.SubroutineCat, returnType: scala.Option[TypeWrap], name: String, params: Seq[Parameter]) extends HeaderNode

  object Subroutine {
    sealed trait SubroutineCat extends HeaderNode

    case object Constructor extends SubroutineCat

    case object Function extends SubroutineCat

    case object Method extends SubroutineCat

    case class Parameter(typ: TypeWrap, name: String) extends HeaderNode
  }

  case class TypeWrap(value: Type) extends HeaderNode

  object TypeWrap {
    val Int: TypeWrap = TypeWrap(Type.Int)
    val Char: TypeWrap = TypeWrap(Type.Char)
    val Boolean: TypeWrap = TypeWrap(Type.Boolean)
    def ClassRef(name: String): TypeWrap = TypeWrap(Type.ClassRef(name))
  }

  sealed trait Util extends HeaderValue

  sealed trait List[+T <: HeaderValue] extends Util with Seq[T] {
  }

  object List {
    case class Cons[+T <: HeaderValue](hd: T, tl: List[T]) extends List[T] {
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

  sealed trait Option[+T <: HeaderValue] extends Util {
    def toOption: scala.Option[T]
  }

  object Option {
    case class Some[+T <: HeaderValue](value: T) extends Option[T] {
      override def toOption: scala.Option[T] = scala.Some(value)
    }

    case object None extends Option[Nothing] {
      override def toOption: scala.Option[Nothing] = scala.None
    }

    def fromOption[T <: HeaderValue](option: scala.Option[T]): Option[T] = option match {
      case scala.Some(value) => Some(value)
      case scala.None => None
    }
  }

  case class Identifier(name: String) extends Util
}
