package com.github.nishi_7

import org.scalatest.funsuite.AnyFunSuite

import scala.io.Source
import ParserValue._

class ParserTest extends AnyFunSuite {
  test("TwoVarClass") {
    assertFromFile("TwoVarClass.jack",
      Class(
        name = "TwoVarClass",
        classVars = Seq(
          ClassVar(ClassVar.Field, Type.Boolean, "fieldVar"),
          ClassVar(ClassVar.Static, Type.Int, "staticVar")
        ),
        subroutines = Seq.empty
      )
    )
  }

  def assertFromFile(path: String, expected: Class) = {
    val source = Source.fromResource(path)
    val result = Parser.parse(Lexer.lex(source.bufferedReader()))
    assert(result == expected)
  }
}
