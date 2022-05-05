package com.github.nishi_7
package front

import jp.pois.pg4scala.common.Token
import jp.pois.pg4scala.lexer.Lexer.TokenGenerator
import jp.pois.pg4scala.lexer.MatchedContext
import jp.pois.pg4scala.lexer.Regex.CharacterClass.{Alphabet, Digit, Word}
import jp.pois.pg4scala.lexer.Regex._

import java.io.Reader

object Lexer {
  private val lexer = jp.pois.pg4scala.lexer.Lexer.builder
    .ignore(' ', '\n', '\t', '\r')
    .rule("//" * characterClass(' ', '\t').rep1 * "@import", Import)
    .ignore("//" * not('\n').rep0 * '\n' * '\r'.opt)
    .ignore("/*" * (not('*') | ('*' * not('/'))).rep0 * "*/")
    .rule('(', LParen)
    .rule(')', RParen)
    .rule('[', LBracket)
    .rule(']', RBracket)
    .rule('{', LBrace)
    .rule('}', RBrace)
    .rule(',', Comma)
    .rule(';', SemiColon)
    .rule('=', Equal)
    .rule('.', Dot)
    .rule('+', Plus)
    .rule('-', Minus)
    .rule('*', Star)
    .rule('/', Slash)
    .rule('&', Amp)
    .rule('|', VertBar)
    .rule('~', Tilde)
    .rule('<', Lt)
    .rule('>', Gt)
    .rule("class", Class)
    .rule("constructor", Constructor)
    .rule("method", Method)
    .rule("function", Function)
    .rule("int", Int)
    .rule("boolean", Boolean)
    .rule("char", Char)
    .rule("void", Void)
    .rule("var", Var)
    .rule("static", Static)
    .rule("field", Field)
    .rule("let", Let)
    .rule("do", Do)
    .rule("if", If)
    .rule("else", Else)
    .rule("while", While)
    .rule("return", Return)
    .rule("true", True)
    .rule("false", False)
    .rule("null", Null)
    .rule("this", This)
    .rule("import", Import)
    .rule(('1' to '9') * Digit.rep0 | '0', {case MatchedContext(str, _, _) => Integer(str.toInt)}: TokenGenerator)
    .rule('"' * not('\n', '"').rep0 * '"', {case MatchedContext(str, _, _) => QuotedString(str.substring(1, str.length - 1))}: TokenGenerator)
    .rule((Alphabet | '_') * (Word | '_').rep0, {case MatchedContext(str, _, _) => Identifier(str)}: TokenGenerator)
    .build

  def lex(reader: Reader): LazyList[Token] = lexer.lex(reader)

  sealed trait JackToken extends Token

  sealed trait SymToken extends JackToken

  case object LParen extends SymToken

  case object RParen extends SymToken

  case object LBracket extends SymToken

  case object RBracket extends SymToken

  case object LBrace extends SymToken

  case object RBrace extends SymToken

  case object Comma extends SymToken

  case object SemiColon extends SymToken

  case object Equal extends SymToken

  case object Dot extends SymToken

  case object Plus extends SymToken

  case object Minus extends SymToken

  case object Star extends SymToken

  case object Slash extends SymToken

  case object Amp extends SymToken

  case object VertBar extends SymToken

  case object Tilde extends SymToken

  case object Lt extends SymToken

  case object Gt extends SymToken

  sealed trait Keyword extends JackToken

  case object Class extends Keyword

  case object Constructor extends Keyword

  case object Method extends Keyword

  case object Function extends Keyword

  sealed trait BuiltinType extends Keyword

  case object Int extends BuiltinType

  case object Boolean extends BuiltinType

  case object Char extends BuiltinType

  case object Void extends BuiltinType

  case object Var extends Keyword

  case object Static extends Keyword

  case object Field extends Keyword

  case object Let extends Keyword

  case object Do extends Keyword

  case object If extends Keyword

  case object Else extends Keyword

  case object While extends Keyword

  case object Return extends Keyword

  case object Import extends Keyword

  case object True extends ConstantValue with Keyword

  case object False extends ConstantValue with Keyword

  case object Null extends ConstantValue with Keyword

  case object This extends Keyword

  sealed trait ConstantValue extends JackToken

  case class Integer(value: Int) extends ConstantValue

  case class QuotedString(value: String) extends ConstantValue

  case class Identifier(name: String) extends JackToken
}
