package com.github.nishi_7

import com.github.nishi_7.front.{Lexer, Parser}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

import scala.io.Source

class ParserTest extends AnyFunSuite {
  test("TwoVarClass") {
    assertFromFile("TwoVarClass.jack",
      Class(
        name = "TwoVarClass",
        classVars = Seq(
          ClassVar(ClassVar.Static, Type.Int, "staticVar"),
          ClassVar(ClassVar.Field, Type.Boolean, "fieldVar"),
          ClassVar(ClassVar.Field, Type.Boolean, "fieldVar2")
        ),
        subroutines = Seq.empty
      )
    )
  }

  test("TestStructure.jack") {
    assertFromFile("TestStructure.jack",
      Class(
        name = "TestStructure",
        classVars = Seq(
          ClassVar(ClassVar.Static, Type.Int, "_sv1"),
          ClassVar(ClassVar.Static, Type.Int, "_sv2"),
          ClassVar(ClassVar.Field, Type.ClassRef("TestStructure"), "ts")
        ),
        subroutines = Seq(
          Subroutine(
            category = Subroutine.Constructor,
            returnType = Some(Type.ClassRef("TestStructure")),
            name = "new",
            params = Seq.empty,
            body = Subroutine.Body(
              vars = Seq.empty,
              statements = Seq(
                Return(Some(This))
              )
            )
          ),
          Subroutine(
            category = Subroutine.Method,
            returnType = None,
            name = "hoge",
            params = Seq(
              Parameter(Type.Int, "intArg"),
              Parameter(Type.Char, "charArg"),
              Parameter(Type.Boolean, "booleanArg")
            ),
            body = Subroutine.Body(
              vars = Seq(
                Var(Type.Boolean, "bl1"),
                Var(Type.Boolean, "bl2")
              ),
              statements = Seq.empty
            )
          ),
          Subroutine(
            category = Subroutine.Function,
            returnType = Some(Type.Boolean),
            name = "pe",
            params = Seq.empty,
            body = Subroutine.Body(
              vars = Seq(
                Var(Type.ClassRef("TestStructure"), "ge"),
                Var(Type.ClassRef("TestStructure"), "go")
              ),
              statements = Seq(
                Statement.Let(
                  name = "ge",
                  index = None,
                  value = ConstantExpression.Integer(1)
                ),
                Statement.Let(
                  name = "go",
                  index = Some(ConstantExpression.Integer(1)),
                  value = ConstantExpression.Integer(2)
                ),
                Statement.If(
                  cond = VarRef("ge"),
                  ifTrue = Seq(
                    Return(Some(ConstantExpression.Integer(1))),
                    Return(Some(ConstantExpression.Integer(2)))
                  ),
                  ifFalse = Seq(
                    Return(Some(ConstantExpression.Integer(1))),
                    Return(Some(ConstantExpression.Integer(2)))
                  )
                ),
                Statement.If(
                  cond = VarRef("ge"),
                  ifTrue = Seq(
                    Return(Some(ConstantExpression.True))
                  ),
                  ifFalse = Seq.empty
                ),
                Statement.While(
                  cond = VarRef("ge"),
                  body = Seq(
                    Return(Some(ConstantExpression.Integer(1))),
                    Return(Some(ConstantExpression.Integer(2)))
                  )
                ),
                Statement.Do(
                  call = SubroutineCall(
                    receiver = Some("TestStructure"),
                    name = "pe",
                    args = Seq.empty
                  )
                ),
                Statement.Do(
                  call = SubroutineCall(
                    receiver = None,
                    name="func",
                    args = Seq(
                      ConstantExpression.Integer(1),
                      ConstantExpression.Integer(2),
                      ConstantExpression.Integer(3)
                    )
                  )
                ),
                Return(Some(ConstantExpression.True)),
                Return(Some(ConstantExpression.False)),
                Return(Some(ConstantExpression.Null)),
                Return(Some(ConstantExpression.This)),
                Return(Some(ConstantExpression.Integer(12345))),
                Return(Some(ConstantExpression.Literal("hello!"))),
                Return(Some(
                  Binary.Or(
                    Binary.And(
                      Binary.Subtract(
                        Binary.Addition(
                          Unary.Inverse(ConstantExpression.Integer(1)),
                          ConstantExpression.Integer(2),
                        ),
                        Binary.Division(
                          Binary.Multiplication(
                            ConstantExpression.Integer(3),
                            ConstantExpression.Integer(4)
                          ),
                          ConstantExpression.Integer(5)
                        )
                      ),
                      ConstantExpression.Integer(6)
                    ),
                    Binary.Eq(
                      Binary.Eq(
                        Binary.Lt(
                          Binary.Gt(
                            ConstantExpression.Integer(7),
                            ConstantExpression.Integer(8),
                          ),
                          ConstantExpression.Integer(9),
                        ),
                        ConstantExpression.Integer(0),
                      ),
                      Binary.Subtract(
                        Binary.Addition(
                          SubroutineCall(
                            receiver = Some("ge"),
                            name = "Way",
                            args = Seq.empty
                          ),
                          ArrayAccess(
                            name = "bo",
                            index = ConstantExpression.Integer(123)
                          )
                        ),
                        Binary.Multiplication(
                          Unary.BitFlip(ConstantExpression.Integer(24)),
                          Unary.Inverse(ConstantExpression.Integer(53))
                        )
                      )
                    )
                  )
                ))
              )
            )
          )
        )
      )
    )
  }

  def assertFromFile(path: String, expected: Class) = {
    val source = Source.fromResource(path)
    val result = Parser.parse(Lexer.lex(source.bufferedReader()))

    assert(result == expected)
  }
}
