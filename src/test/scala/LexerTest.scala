package com.github.nishi_7

import com.github.nishi_7.front.Lexer
import com.github.nishi_7.front.Lexer._
import jp.pois.pg4scala.common.Token
import jp.pois.pg4scala.common.Token.EOF
import jp.pois.pg4scala.lexer.exceptions.MismatchedCharException
import org.scalatest.funsuite.AnyFunSuite

import java.io.StringReader

class LexerTest extends AnyFunSuite {
  test("Ignore") {
    assertLex(
"""
                       """)()
    assertLex(
""" // test!!! thistest!!!
function // yoyo!
""")(Function)

    assertLex(
"""/* Comment out!!

here


here!!
 */ function
""".stripMargin
    )(Function)

    assertLex(
""" /** Document!!!

Ignore me !
*/ function
""")(Function)
  }

  test("Symbols") {
    assertLex("()")(LParen, RParen)
    assertLex("[]")(LBracket, RBracket)
    assertLex("{}")(LBrace, RBrace)
    assertLex(",")(Comma)
    assertLex(";")(SemiColon)
    assertLex("=")(Equal)
    assertLex(".")(Dot)
    assertLex("+")(Plus)
    assertLex("-")(Minus)
    assertLex("*")(Star)
    assertLex("/")(Slash)
    assertLex("&")(Amp)
    assertLex("|")(VertBar)
    assertLex("~")(Tilde)
    assertLex("<")(Lt)
    assertLex(">")(Gt)
  }

  test("PreservedKeywords") {
    assertLex("class")(Class)
    assertLex("constructor")(Constructor)
    assertLex("method")(Method)
    assertLex("function")(Function)

    assertLex("int")(Int)
    assertLex("boolean")(Boolean)
    assertLex("char")(Char)
    assertLex("void")(Void)

    assertLex("var")(Var)
    assertLex("static")(Static)
    assertLex("field")(Field)

    assertLex("let")(Let)
    assertLex("do")(Do)
    assertLex("if")(If)
    assertLex("else")(Else)
    assertLex("while")(While)
    assertLex("return")(Return)

    assertLex("true")(True)
    assertLex("false")(False)
    assertLex("null")(Null)

    assertLex("this")(This)
  }

  test("Literals") {
    assertLex("1984")(Integer(1984))
    assertLex("-13")(Minus, Integer(13))
    assertLex("""""""")(QuotedString(""))
    assertLex(""""hello, world"""")(QuotedString("hello, world"))
    assertLex(""""こんにちは，世界"""")(QuotedString("こんにちは，世界"))

    assertNqLex("0123")(Integer(123))
    assertFail("\"hello!\nworld\"")
  }

  test("Identifier") {
    assertLex("println")(Identifier("println"))
    assertLex("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_")(Identifier("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_"))
    assertLex("__identifier")(Identifier("__identifier"))

    assertNqLex("123Identifier")(Identifier("123Identifier"))
  }

  def lexString(str: String): LazyList[Token] = Lexer.lex(new StringReader(str))

  def assertLex(str: String)(expected: Lexer.JackToken*): Unit = assert(lexString(str) == expected ++ Seq(EOF))

  def assertNqLex(str: String)(notExpected: Lexer.JackToken*): Unit = assert(lexString(str) != notExpected ++ Seq(EOF))

  def assertFail(str: String): Unit = assertThrows[MismatchedCharException](lexString(str))
}
