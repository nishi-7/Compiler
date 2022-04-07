package com.github.nishi_7

import Lexer.Identifier
import Parser.Symbols._
import ParserValue.ClassVar.{Field, Static, VarType}
import ParserValue.ReferenceExpression.{ArrayAccess, VarRef}
import ParserValue.Type.ClassRef
import ParserValue.{Binary, Class, ClassVar, ConstantExpression, Expression, ReferenceExpression, Subroutine, Unary}

import jp.pois.pg4scala.common.Token
import jp.pois.pg4scala.parser.Character._
import jp.pois.pg4scala.parser.NonTerminalSymbol
import jp.pois.pg4scala.parser.Parser.RGParam

//noinspection TypeAnnotation
object Parser {
  def parse(tokens: LazyList[Token]): Class = parser.parse(tokens).asInstanceOf[Class]

  private type DefaultRGP = RGParam[ParserValue]
  private type PVList[+T <: ParserValue] = ParserValue.List[T]
  private type PVOption[+T <: ParserValue] = ParserValue.Option[T]

  private val parser = {
    val IdentifierToken = Terminal(Lexer.Identifier.getClass)
    val IntegerToken = Terminal(Lexer.Integer.getClass)
    val QuotedStringToken = Terminal(Lexer.QuotedString.getClass)

    val identifierGenerator = { seq: DefaultRGP => ParserValue.Identifier(seq.head.asTokenOf[Lexer.Identifier].name) }
    val passThroughHead = { seq: DefaultRGP => seq.head.asValue }

    jp.pois.pg4scala.parser.Parser.builder[ParserValue](Root)
      .rule(Root, charSeq(Lexer.Class, IdentifierToken, Lexer.LBrace, ClassVarDec, SubroutineDec, Lexer.RBrace),
        { seq => Class(seq(1).asTokenOf[Identifier].name, seq(3).asValueOf[PVList[ClassVar]], seq(4).asValueOf[PVList[Subroutine]]) }
      )
      .ruleRep0(ClassVarDec, charSeq(StaticOrField, Type, VarNameRep, Lexer.SemiColon),
        { seq: DefaultRGP =>
          val varType = seq.head.asValueOf[VarType]
          val typ = seq(1).asValueOf[ParserValue.Type]
          seq(2).asValueOf[PVList[ParserValue.Identifier]]
            .map { id => ClassVar(varType, typ, id.name) }
            .foldRight[PVList[ClassVar]](ParserValue.List.Empty)(ParserValue.List.Cons[ClassVar])
        },
        { (cur: PVList[ClassVar], acc: PVList[ClassVar]) => cur.foldRight(acc)(ParserValue.List.Cons[ClassVar]) },
        ParserValue.List.Empty
      )
      .rule(StaticOrField, charSeq(Lexer.Static), { _ => Static })
      .rule(StaticOrField, charSeq(Lexer.Field), { _ => Field })
      .rule(Type, charSeq(Lexer.Int), { _ => ParserValue.Type.Int })
      .rule(Type, charSeq(Lexer.Char), { _ => ParserValue.Type.Char })
      .rule(Type, charSeq(Lexer.Boolean), { _ => ParserValue.Type.Boolean })
      .rule(Type, charSeq(IdentifierToken), { seq => ClassRef(seq.head.asTokenOf[Lexer.Identifier].name) })
      .ruleRep1(VarNameRep, charSeq(VarName), charSeq(Lexer.Comma),
        { seq: DefaultRGP => seq.head.asValueOf[ParserValue.Identifier] },
        { (hd: ParserValue.Identifier, tl: PVList[ParserValue.Identifier]) => ParserValue.List.Cons(hd, tl) },
        ParserValue.List.Empty
      )
      .rule(VarName, charSeq(IdentifierToken), identifierGenerator)
      .ruleRep0(SubroutineDec,
        charSeq(SubroutineCat, ReturnType, SubroutineName, Lexer.LParen, Parameter, Lexer.RParen, SubroutineBody),
        { seq: DefaultRGP =>
          Subroutine(
            seq.head.asValueOf[Subroutine.SubroutineCat],
            seq(1).asValueOf[ParserValue.Option[ParserValue.Type]].toOption,
            seq(2).asValueOf[ParserValue.Identifier].name,
            seq(4).asValueOf[PVList[Subroutine.Parameter]],
            seq(6).asValueOf[Subroutine.Body]
          )
        },
        { (hd: Subroutine, tl: PVList[Subroutine]) => ParserValue.List.Cons(hd, tl) },
        ParserValue.List.Empty)
      .rule(SubroutineCat, charSeq(Lexer.Constructor), { _ => ParserValue.Subroutine.Constructor })
      .rule(SubroutineCat, charSeq(Lexer.Function), { _ => ParserValue.Subroutine.Function })
      .rule(SubroutineCat, charSeq(Lexer.Method), { _ => ParserValue.Subroutine.Method })
      .rule(ReturnType, charSeq(Type), { seq: DefaultRGP => ParserValue.Option.Some(seq.head.asValue) })
      .rule(ReturnType, charSeq(Lexer.Void), { _ => ParserValue.Option.None })
      .rule(SubroutineName, charSeq(IdentifierToken), identifierGenerator)
      .ruleRep0(Parameter, charSeq(Type, IdentifierToken), charSeq(Lexer.Comma),
        { seq: DefaultRGP => Subroutine.Parameter(seq.head.asValueOf[ParserValue.Type], seq(1).asTokenOf[Lexer.Identifier].name) },
        { (hd: Subroutine.Parameter, tl: PVList[Subroutine.Parameter]) => ParserValue.List.Cons(hd, tl) },
        ParserValue.List.Empty
      )
      .rule(SubroutineBody, charSeq(Lexer.LBrace, VarDec, Statements, Lexer.RBrace),
        { seq => Subroutine.Body(seq(1).asValueOf[PVList[ParserValue.Var]], seq(2).asValueOf[PVList[ParserValue.Statement]]) })
      .rule(VarDec, charSeq(Lexer.Var, Type, VarNameRep, Lexer.Comma),
        { seq =>
          val typ = seq(1).asValueOf[ParserValue.Type]
          seq(2).asValueOf[ParserValue.List[ParserValue.Identifier]]
            .map { id => ParserValue.Var(typ, id.name) }
            .foldRight[PVList[ParserValue.Var]](ParserValue.List.Empty) { (hd, tl) => ParserValue.List.Cons(hd, tl) }
        }
      )
      .ruleRep0(Statements, charSeq(Statement),
        { seq: DefaultRGP => seq.head.asValueOf[PVList[ParserValue.Statement]] },
        { (cur: PVList[ParserValue.Statement], acc: PVList[ParserValue.Statement]) => cur.foldRight(acc)(ParserValue.List.Cons[ParserValue.Statement]) },
        ParserValue.List.Empty
      )
      .rule(Statement, charSeq(LetStatement), passThroughHead)
      .rule(Statement, charSeq(IfStatement), passThroughHead)
      .rule(Statement, charSeq(WhileStatement), passThroughHead)
      .rule(Statement, charSeq(DoStatement), passThroughHead)
      .rule(Statement, charSeq(ReturnStatement), passThroughHead)
      .rule(LetStatement, charSeq(Lexer.Let, VarName, ArrayIndex, Lexer.Equal, Expression, Lexer.SemiColon),
        { seq =>
          ParserValue.Statement.Let(
            seq(1).asValueOf[ParserValue.Identifier].name,
            seq(2).asValueOf[PVOption[Expression]].toOption,
            seq(4).asValueOf[Expression]
          )
        }
      )
      .ruleOpt(ArrayIndex, charSeq(Lexer.LBracket, Expression, Lexer.RBracket), { seq => seq(1).asValue },
        { opt: Option[ParserValue] => ParserValue.Option.fromOption(opt) }
      )
      .rule(IfStatement, charSeq(Lexer.If, Lexer.LParen, Expression, Lexer.RParen, Lexer.LBrace, Statements, Lexer.RBrace, ElseOpt),
        { seq => ParserValue.Statement.If(seq(2).asValueOf[Expression], seq(5).asValueOf[PVList[ParserValue.Statement]], seq(7).asValueOf[PVList[ParserValue.Statement]]) }
      )
      .ruleOpt(ElseOpt, charSeq(Lexer.Else, Lexer.LBrace, Statements, Lexer.RBrace), { seq => seq(2).asValueOf[PVList[ParserValue.Statement]] },
        { opt: Option[PVList[ParserValue.Statement]] =>
          opt match {
            case Some(value) => value
            case None => ParserValue.List.Empty
          }
        }
      )
      .rule(WhileStatement, charSeq(Lexer.While, Lexer.LParen, Expression, Lexer.RParen, Lexer.LBrace, Statements, Lexer.RBrace),
        { seq => ParserValue.Statement.While(seq(2).asValueOf[Expression], seq(5).asValueOf[PVList[ParserValue.Statement]]) }
      )
      .rule(DoStatement, charSeq(Lexer.Do, SubroutineCall, Lexer.SemiColon), { seq => seq(1).asValue })
      .rule(ReturnStatement, charSeq(Lexer.Return, ExpressionOpt, Lexer.SemiColon),
        { seq => ParserValue.Statement.Return(seq(1).asValueOf[PVOption[Expression]].toOption) }
      )
      .ruleOpt(ExpressionOpt, charSeq(Expression),
        { seq => seq.head.asValueOf[Expression] },
        { opt: Option[Expression] => ParserValue.Option.fromOption(opt) }
      )
      .rule(Expression, charSeq(Term7), passThroughHead)
      .rule(Term7, charSeq(Term6), passThroughHead)
      .rule(Term7, charSeq(Term7, Lexer.VertBar, Term6), genBinary(Binary.Or))
      .rule(Term6, charSeq(Term5), passThroughHead)
      .rule(Term6, charSeq(Term6, Lexer.Amp, Term5), genBinary(Binary.And))
      .rule(Term5, charSeq(Term4), passThroughHead)
      .rule(Term5, charSeq(Term5, Lexer.Equal, Term4), genBinary(Binary.Eq))
      .rule(Term4, charSeq(Term3), passThroughHead)
      .rule(Term4, charSeq(Term4, Lexer.Gt, Term3), genBinary(Binary.Gt))
      .rule(Term4, charSeq(Term4, Lexer.Lt, Term3), genBinary(Binary.Lt))
      .rule(Term3, charSeq(Term3), passThroughHead)
      .rule(Term3, charSeq(Term3, Lexer.Plus, Term2), genBinary(Binary.Addition))
      .rule(Term3, charSeq(Term3, Lexer.Minus, Term2), genBinary(Binary.Subtract))
      .rule(Term2, charSeq(Term2), passThroughHead)
      .rule(Term2, charSeq(Term2, Lexer.Star, Term1), genBinary(Binary.Multiplication))
      .rule(Term2, charSeq(Term2, Lexer.Slash, Term1), genBinary(Binary.Division))
      .rule(Term1, charSeq(Term0), passThroughHead)
      .rule(Term1, charSeq(Lexer.Minus, Term1), { seq => Unary.Inverse(seq(1).asValueOf[Expression]) })
      .rule(Term1, charSeq(Lexer.Tilde, Term1), { seq => Unary.BitFlip(seq(1).asValueOf[Expression]) })
      .rule(Term0, charSeq(QuotedStringToken), { seq => ConstantExpression.Literal(seq.head.asTokenOf[Lexer.QuotedString].value) })
      .rule(Term0, charSeq(IntegerToken), { seq => ConstantExpression.Integer(seq.head.asTokenOf[Lexer.Integer].value) })
      .rule(Term0, charSeq(KeywordConstant), passThroughHead)
      .rule(Term0, charSeq(VarName), { seq => VarRef(seq.head.asValueOf[ParserValue.Identifier].name) })
      .rule(Term0, charSeq(SubroutineCall), passThroughHead)
      .rule(Term0, charSeq(Lexer.LParen, Expression, Lexer.RParen), { seq => seq(1).asValue })
      .rule(Term0, charSeq(VarName, Lexer.LBracket, Expression, Lexer.RBracket),
        { seq => ArrayAccess(seq.head.asValueOf[ParserValue.Identifier].name, seq(2).asValueOf[Expression]) }
      )
      .rule(SubroutineCall, charSeq(SubroutineName, Lexer.LParen, ArgumentList, Lexer.RParen),
        { seq =>
          ReferenceExpression.SubroutineCall(
            None,
            seq.head.asValueOf[ParserValue.Identifier].name,
            seq(2).asValueOf[PVList[Expression]]
          )
        }
      )
      .rule(SubroutineCall, charSeq(IdentifierToken, Lexer.Dot, SubroutineName, Lexer.LParen, ArgumentList, Lexer.RParen),
        { seq =>
          ReferenceExpression.SubroutineCall(
            Some(seq.head.asTokenOf[Lexer.Identifier].name),
            seq(2).asValueOf[ParserValue.Identifier].name,
            seq(4).asValueOf[PVList[Expression]]
          )
        }
      )
      .ruleRep0(ArgumentList, charSeq(Expression), charSeq(Lexer.Comma), passThroughHead,
        { (hd: ParserValue, tl: PVList[ParserValue]) => ParserValue.List.Cons(hd, tl) },
        ParserValue.List.Empty
      )
      .rule(KeywordConstant, charSeq(Lexer.True), { _ => ConstantExpression.True })
      .rule(KeywordConstant, charSeq(Lexer.False), { _ => ConstantExpression.False })
      .rule(KeywordConstant, charSeq(Lexer.Null), { _ => ConstantExpression.Null })
      .rule(KeywordConstant, charSeq(Lexer.This), { _ => ConstantExpression.This })
      .build
  }

  private def genBinary(f: (Expression, Expression) => Expression)(seq: DefaultRGP) = {
    f(seq.head.asValueOf[Expression], seq(2).asValueOf[Expression])
  }

  object Symbols {
    val Root = NonTerminalSymbol("ProgramStructure")
    val ClassVarDec = NonTerminalSymbol("ClassVarDec")
    val SubroutineDec = NonTerminalSymbol("SubroutineDec")
    val StaticOrField = NonTerminalSymbol("StaticOrField")
    val Type = NonTerminalSymbol("Type")
    val VarNameRep = NonTerminalSymbol("VarNameRep")
    val VarName = NonTerminalSymbol("VarName")
    val SubroutineCat = NonTerminalSymbol("FunctionCat")
    val ReturnType = NonTerminalSymbol("ReturnType")
    val SubroutineName = NonTerminalSymbol("SubroutineName")
    val Parameter = NonTerminalSymbol("Parameter")
    val SubroutineBody = NonTerminalSymbol("SubroutineBody")
    val VarDec = NonTerminalSymbol("VarDec")

    val Statements = NonTerminalSymbol("Statements")
    val Statement = NonTerminalSymbol("Statement")
    val LetStatement = NonTerminalSymbol("LetStatement")
    val IfStatement = NonTerminalSymbol("IfStatement")
    val WhileStatement = NonTerminalSymbol("WhileStatement")
    val DoStatement = NonTerminalSymbol("DoStatement")
    val ReturnStatement = NonTerminalSymbol("ReturnStatement")
    val ArrayIndex = NonTerminalSymbol("ArrayIndexOpt")
    val ElseOpt = NonTerminalSymbol("ElseOpt")
    val ExpressionOpt = NonTerminalSymbol("ExpressionOpt")

    val Expression = NonTerminalSymbol("Expression")
    val Term0 = NonTerminalSymbol("Term0") // ( exp ), constant, function call and array access
    val Term1 = NonTerminalSymbol("Term1") // unary -, ~
    val Term2 = NonTerminalSymbol("Term2") // *, /
    val Term3 = NonTerminalSymbol("Term3") // +, -
    val Term4 = NonTerminalSymbol("Term4") // <. >
    val Term5 = NonTerminalSymbol("Term5") // =
    val Term6 = NonTerminalSymbol("Term6") // &
    val Term7 = NonTerminalSymbol("Term7") // |
    val SubroutineCall = NonTerminalSymbol("SubroutineCall")
    val ArgumentList = NonTerminalSymbol("ExpressionList")
    val Receiver = NonTerminalSymbol("Receiver")

    val UnaryOp = NonTerminalSymbol("UnaryOp")
    val KeywordConstant = NonTerminalSymbol("KeywordConstant")
  }
}
