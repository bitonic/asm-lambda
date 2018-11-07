package org.francesco.asmlambda.compiler

import scala.Function.const
import scala.collection.mutable.ArraySeq
import scala.language.postfixOps

import Syntax.Expr
import Syntax.{Expr => E}
import Syntax.Prim
import Syntax.Definition
import Syntax.Package

object Parser {
  import fastparse._
  import ScalaWhitespace._

  def alphaNum[_: P]: P[Unit] = CharIn("a-zA-Z0-9")
  def alpha[_: P]: P[Unit] = CharIn("a-zA-Z")
  def digit[_: P]: P[Unit] = CharIn("0-9")
  def stringChars(c: Char): Boolean = c != '\"' && c != '\\'
  def strChars[_: P]: P[Unit] = P(CharsWhile(stringChars))
  def hexDigit[_: P]: P[Unit] =CharIn("0-9a-fA-F")

  def unicodeEscape[_: P]: P[String] =
    "\\u" ~~ (hexDigit ~~ hexDigit ~~ hexDigit ~~ hexDigit).!
      .map(i => new String(Character.toChars(Integer.parseInt(i, 16))))

  def escape[_: P]: P[String] =
    "\\" ~~ CharPred(ch => ch == '"' || ch == '\\' || ch == 'n' || ch == 't').!.map {
      case "\"" => "\""
      case "\\" => "\\"
      case "n" => "\n"
      case "t" => "\t"
    }

  // keep in sync with PrimOp.escapeString
  def string[_: P]: P[String] =
    "\"" ~~/ (strChars.! | escape | unicodeEscape).repX.map(_.mkString) ~~ "\"" /

  def reservedSet: Set[String] = Set("if", "def", "true", "false", "let", "then", "else", "main")

  def recordLabel[_: P]: P[String] =
    alphaNum.repX(1).!.flatMap(lbl => if (reservedSet.contains(lbl)) { Fail } else { Pass(lbl)})

  def recordLookup[_: P]: P[String] = "." ~~ recordLabel

  def recordLit[_: P]: P[Seq[(String, Expr)]] =
    "{" ~ (recordLabel ~ "=" ~/ expr).rep(0, ",") ~ "}" /

  def recordUpdate[_: P](rec: Expr): P[Expr] =
    recordLit.map(_.foldRight(rec) { case ((lbl, body), rec) => E.RecordUpdate(rec, lbl, body) })

  def variable[_: P]: P[String] =
    (alpha ~~ alphaNum.repX).!.flatMap(v => if (reservedSet.contains(v)) { Fail } else { Pass(v) })

  def arguments[_: P]: P[ArraySeq[Expr]] =
    "(" ~ expr1.rep(0, ",").map(args => ArraySeq(args: _*)) ~ ")" /

  def prim[_: P]: P[Prim] =
    (digit.repX(1) ~~ "." ~~ digit.rep(1)).!.map(i => Prim.F64(i.toDouble)) |
    digit.repX(1).!.map(i => Prim.I64(i.toLong)) |
    P("true").map(const(Prim.Bool(true))) |
    P("false").map(const(Prim.Bool(false))) |
    string.map(Prim.Text)

  def record[_: P]: P[Map[String, Expr]] = recordLit.map(kvs => Map(kvs: _*))

  def array[_: P]: P[Seq[Expr]] =
    "[" ~/ expr.rep(0, ",") ~ "]" /

  def expr6[_: P]: P[Expr] =
    prim.map(E.Prim) |
    record.map(E.Record) |
    array.map(els => E.Array(ArraySeq(els: _*))) |
    variable.map(E.Var) |
    ("(" ~ expr ~ ")")

  def arrayGet[_: P]: P[Expr] = {
    "[" ~/ expr ~ "]" /
  }

  def expr5[_: P]: P[Expr] = {
    def go(e: Expr): P[Expr] =
      recordLit.flatMap(upd => go(upd.foldLeft(e){ case (rec, (lbl, body)) => E.RecordUpdate(rec, lbl, body) })) |
      arguments.flatMap(args => go(E.App(e, args))) |
      recordLookup.flatMap(lbl => go(E.RecordLookup(e, lbl))) |
      arrayGet.flatMap(ix => go(E.mkApp(E.PrimOp.arrGet, e, ix))) |
      Pass(e)
    expr6.flatMap(go)
  }

  def expr4[_: P]: P[Expr] = {
    def go(e1: Expr): P[Expr] =
      ("*" ~/ expr5.flatMap(e2 => go(E.mkApp(E.PrimOp.mul, e1, e2)))) |
      ("/" ~/ expr5.flatMap(e2 => go(E.mkApp(E.PrimOp.div, e1, e2)))) |
      Pass(e1)
    expr5.flatMap(go)
  }

  def expr3[_: P]: P[Expr] = {
    def go(e1: Expr): P[Expr] =
      ("+" ~/ expr4.flatMap(e2 => go(E.mkApp(E.PrimOp.add, e1, e2)))) |
      ("-" ~/ expr4.flatMap(e2 => go(E.mkApp(E.PrimOp.sub, e1, e2)))) |
      Pass(e1)
    expr4.flatMap(go)
  }

  def `def`[_: P]: P[(String, Definition)] =
    ("def" ~/ variable ~/ "(" ~/ variable.rep(0, ",") ~/ ")" ~/ "=" ~/ expr ~/ ";")
      .map{ case (v, args, body) => (v, Definition(ArraySeq(args: _*), body)) }
      // TODO adding the above breaks the parser compilation...
      // .flatMap(defn => if (defn._1 == "main") { Fail } else { Pass(defn) })

  def expr2[_: P]: P[Expr] = {
    def go(e1: Expr): P[Expr] =
      ("==" ~/ expr3.flatMap(e2 => go(E.mkApp(E.PrimOp.eq, e1, e2)))) |
      Pass(e1)
    expr3.flatMap(go)
  }

  def expr1[_: P]: P[Expr] =
    ("\\" ~/ "(" ~/ variable.rep(0, ",") ~/ ")" ~/ "->" ~/ expr)
        .map{ case (args, body) => E.Lam(ArraySeq(args: _*), body) } |
    ("if" ~/ expr ~/ "then" ~/ expr ~/ "else" ~/ expr)
        .map{ case (cond, l, r) => E.ITE(cond, l, r) } |
    ("let" ~/ variable ~/ "=" ~/ expr ~/ ";" ~/ expr)
        .map{ case (v, bound, body) => E.Let(v, bound, body)} |
    (`def` ~/ expr).map{ case (v, defn, body) => E.Def(v, defn, body) } |
    expr2

  def expr[_: P]: P[Expr] = expr1

  def exprOnly[_: P]: P[Expr] = Pass(()) ~ expr ~ End

  def `package`[_: P]: P[Package] = Pass(()) ~ (`def`.rep ~ expr ~ End).map{
    case (defs, e) => Package(Map(defs: _*), e)
  }
}
