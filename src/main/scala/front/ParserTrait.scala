package com.github.nishi_7
package front

import jp.pois.pg4scala.common.Token
import jp.pois.pg4scala.parser.Parser

trait ParserTrait[PV, TopValue <: PV] {
  def parse(tokens: LazyList[Token]): TopValue = parser.parse(tokens).asInstanceOf[TopValue]

  protected val parser: Parser[PV]
}
