package com.github.nishi_7
package front

import front.HeaderValue.Subroutine.Parameter
import front.HeaderValue.{Class, HeaderFile, Identifier, Subroutine, TypeWrap}

import jp.pois.pg4scala.parser.Character.{Terminal, charSeq}
import jp.pois.pg4scala.parser.Parser.RGParam
import jp.pois.pg4scala.parser.{NonTerminalSymbol, Parser}

object HeaderParser extends ParserTrait[HeaderValue, HeaderValue] {
  //noinspection ZeroIndexToHead
  override protected val parser: Parser[HeaderValue] = {
    val IdentifierToken = Terminal(classOf[Lexer.Identifier])

    type DefaultRGP = RGParam[HeaderValue]
    type PVList[+T <: HeaderValue] = HeaderValue.List[T]
    type PVOption[+T <: HeaderValue] = HeaderValue.Option[T]

    jp.pois.pg4scala.parser.Parser.builder[HeaderValue](HeaderSymbols.Root)
      .rule(HeaderSymbols.Root, charSeq(HeaderSymbols.Import, HeaderSymbols.Class),
        { seq: DefaultRGP => HeaderFile(seq(0).asValueOf, seq(1).asValueOf) }
      )
      .ruleRep0(HeaderSymbols.Import, charSeq(Lexer.Import, HeaderSymbols.Identifiers, Lexer.SemiColon),
        { seq: DefaultRGP => seq(1).asValueOf[PVList[Identifier]] },
        { (cur: PVList[Identifier], acc: PVList[Identifier]) => cur.foldRight(acc)(HeaderValue.List.Cons[Identifier]) },
        HeaderValue.List.Empty
      )
      .rule(HeaderSymbols.Class, charSeq(Lexer.Class, IdentifierToken, Lexer.LBrace, HeaderSymbols.SrDef, Lexer.RBrace),
        { seq: DefaultRGP => Class(seq(1).asValueOf[Identifier].name, seq(3).asValueOf[PVList[Subroutine]]) },
      )
      .ruleRep0(HeaderSymbols.SrDef,
        charSeq(HeaderSymbols.SrCat, HeaderSymbols.ReturnTyp, IdentifierToken, Lexer.LParen, HeaderSymbols.Params, Lexer.RParen, Lexer.SemiColon),
        { seq: DefaultRGP =>
          Subroutine(
            seq(0).asValueOf[Subroutine.SubroutineCat],
            seq(1).asValueOf[PVOption[TypeWrap]].toOption,
            seq(2).asTokenOf[Lexer.Identifier].name,
            seq(4).asValueOf[PVList[Parameter]]
          )
        },
        HeaderValue.List.Cons[Subroutine], HeaderValue.List.Empty
      )
      .rule(HeaderSymbols.SrCat, charSeq(Lexer.Function), { _ => Subroutine.Function })
      .rule(HeaderSymbols.SrCat, charSeq(Lexer.Constructor), { _ => Subroutine.Constructor })
      .rule(HeaderSymbols.SrCat, charSeq(Lexer.Method), { _ => Subroutine.Method })
      .rule(HeaderSymbols.ReturnTyp, charSeq(HeaderSymbols.Typ), { seq: DefaultRGP => HeaderValue.Option.Some(seq(0).asValueOf[TypeWrap]) })
      .rule(HeaderSymbols.ReturnTyp, charSeq(Lexer.Void), { _ => HeaderValue.Option.None })
      .ruleRep0(HeaderSymbols.Params, charSeq(HeaderSymbols.Typ, IdentifierToken), charSeq(Lexer.SemiColon),
        { seq: DefaultRGP => Parameter(seq(0).asValueOf[TypeWrap], seq(1).asTokenOf[Lexer.Identifier].name) },
        HeaderValue.List.Cons[Parameter], HeaderValue.List.Empty
      )
      .rule(HeaderSymbols.Typ, charSeq(Lexer.Int), { _ => TypeWrap.Int })
      .rule(HeaderSymbols.Typ, charSeq(Lexer.Char), { _ => TypeWrap.Char })
      .rule(HeaderSymbols.Typ, charSeq(Lexer.Boolean), { _ => TypeWrap.Boolean })
      .rule(HeaderSymbols.Typ, charSeq(IdentifierToken),
        { seq: DefaultRGP => TypeWrap.ClassRef(seq(1).asTokenOf[Lexer.Identifier].name) }
      )
      .ruleRep1(HeaderSymbols.Identifiers, charSeq(IdentifierToken), charSeq(Lexer.Comma),
        { seq: DefaultRGP => Identifier(seq(0).asTokenOf[Lexer.Identifier].name) },
        HeaderValue.List.Cons[Identifier], HeaderValue.List.Empty
      )
      .build
  }
}

//noinspection TypeAnnotation
private object HeaderSymbols {
  val Root = NonTerminalSymbol("Root")
  val Import = NonTerminalSymbol("Import")
  val Class = NonTerminalSymbol("Class")
  val SrDef = NonTerminalSymbol("SrDef")
  val SrCat = NonTerminalSymbol("SrCat")
  val ReturnTyp = NonTerminalSymbol("ReturnTyp")
  val Params = NonTerminalSymbol("Params")
  val Typ = NonTerminalSymbol("Typ")
  val Identifiers = NonTerminalSymbol("Identifiers")
}
