package com.github.nishi_7
package mid.util

import back.RawASTNode.Location
import back.RawASTNode.Location.{Argument, Field, Local, Static}
import back.SymbolTable
import back.SymbolTable.{FieldVar, LocalVar, StaticVar}
import back.exception.UndefinedVariableException

trait SymbolTableMixin {
  protected val symTable = new SymbolTable

  protected def symTableVarToLocation: SymbolTable.Var => Location = {
    case StaticVar(_, index) => Static(index)
    case FieldVar(_, index) => Field(index)
    case SymbolTable.Argument(_, index) => Argument(index)
    case LocalVar(_, index) => Local(index)
  }
}
