package com.github.nishi_7
package back

import back.exception.VMWriter.Command

object RawASTNode {
  case class Subroutine(className: String, subroutineName: String, localVarSize: Int, body: Block)

  sealed abstract class Statement

  object Statement {
    case class Assignment(assignTo: Location, value: Expression) extends Statement

    case class If(cond: Expression, ifTrue: Block, ifFalse: Block) extends Statement

    case class While(cond: Expression, body: Block) extends Statement

    case class Do(content: SubroutineCall) extends Statement

    case class Return(value: Option[Expression]) extends Statement
  }

  type Block = Seq[Statement]

  sealed abstract class Expression

  case class LocationValue(location: Location) extends Expression

  case class CommandCall(rator: Command, rands: Seq[Expression]) extends Expression

  case class SubroutineCall(className: String, subroutineName: String, args: Seq[Expression]) extends Expression

  case class Literal(str: String) extends Expression

  sealed abstract class Location

  object Location {
    case class Constant(index: Int) extends Location
    case class Static(index: Int) extends Location
    case class Field(index: Int) extends Location
    case class Argument(index: Int) extends Location
    case class Local(index: Int) extends Location
    case class ArrayWithIndex(head: Location, index: Expression) extends Location
    case object ThisPointer extends Location
  }
}
