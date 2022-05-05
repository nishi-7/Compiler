package com.github.nishi_7
package back.exception

import front.Type

class IllegalComparisonException(lhsType: Type, rhsType: Type) extends N2TCompilerException(
  s"""The program tries to compare values type-illegally: type1: $lhsType, type2: $rhsType"""
)
