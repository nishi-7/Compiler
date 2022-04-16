package com.github.nishi_7
package backend

import backend.SymbolTable._

import com.github.nishi_7.front.ParserValue
import com.github.nishi_7.front.ParserValue.Type

import scala.collection.mutable

class SymbolTable {
  private val classVars: mutable.Map[String, ClassVar] = mutable.Map()
  private var staticCount = 0
  private var fieldCount = 0
  private val locals: mutable.Map[String, SubroutineVar] = mutable.Map()
  private var argumentCount = 0
  private var localCount = 0

  def startStaticSubroutine(): Unit = {
    locals.clear()
    argumentCount = 0
    localCount = 0
  }

  def startMethod(): Unit = {
    locals.clear()
    argumentCount = 1
    localCount = 0
  }

  def define(v: ParserValue.ClassVar): Unit = v.varType match {
    case ParserValue.ClassVar.Static => {
      classVars(v.name) = StaticVar(v.typ, staticCount)
      staticCount += 1
    }
    case ParserValue.ClassVar.Field => {
      classVars(v.name) = FieldVar(v.typ, fieldCount)
      fieldCount += 1
    }
  }

  def define(v: ParserValue.Subroutine.Parameter): Unit = {
    locals(v.name) = Argument(v.typ, argumentCount)
    argumentCount += 1
  }

  def define(v: ParserValue.Var): Unit = {
    locals(v.name) = LocalVar(v.typ, localCount)
    localCount += 1
  }

  def apply(name: String): Option[Var] = classVars.get(name).orElse { locals.get(name) }
}

object SymbolTable {
  sealed trait Var {
    def typ: Type
    def index: Int
  }

  sealed trait ClassVar extends Var
  case class StaticVar(typ: Type, index: Int) extends ClassVar
  case class FieldVar(typ: Type, index: Int) extends ClassVar

  sealed trait SubroutineVar extends Var
  case class Argument(typ: Type, index: Int) extends SubroutineVar
  case class LocalVar(typ: Type, index: Int) extends SubroutineVar
}
