package org.francesco.asmlambda

class Parser(val input: org.parboiled2.ParserInput) extends org.parboiled2.Parser {
  import org.parboiled2._
  import shapeless.{::, HNil}
  import org.francesco.asmlambda.{Syntax => S}
  import org.francesco.asmlambda.Syntax.{Expr => E}

  def WS: Rule0 = rule { zeroOrMore(anyOf(" \t \n")) }

  def tk(s: String): Rule0 = rule { str(s) ~ WS }
  def tk(c: Char): Rule0 = rule { ch(c) ~ WS }

  val reserved: Set[String] = Set("if", "def", "true", "false", "let", "then", "else")

  def Reserved(s: String): Rule0 = {
    assert(reserved.contains(s), s"$s is not reserved word")
    rule { tk(s) }
  }

  def RecordLabel: Rule1[String] = rule {
    capture(oneOrMore(CharPredicate.AlphaNum)) ~ WS
  }

  def Lookup: Rule[S.Expr :: HNil, S.Expr :: HNil] = rule {
    tk('.') ~ RecordLabel ~> E.Lookup
  }

  def Variable: Rule1[String] = rule {
    capture(CharPredicate.Alpha ~ zeroOrMore(CharPredicate.AlphaNum)) ~ WS ~>
      ((s: String) => test(!reserved.contains(s)) ~ push(s))
  }

  def Argument: Rule1[S.Expr] = rule {
    Expr5 ~ zeroOrMore(Lookup)
  }

  def Prim: Rule1[S.Prim] = rule {
    capture(oneOrMore(CharPredicate.Digit)) ~ WS ~> (i => S.Prim.Int64(i.toLong)) |
      Reserved("true") ~ push(S.Prim.Bool(true)) |
      Reserved("false") ~ push(S.Prim.Bool(false))
      // TODO escaped string
  }

  def Record: Rule1[Map[String, S.Expr]] = rule {
    tk('{') ~
      oneOrMore(RecordLabel ~ tk('=') ~ Expr ~> ((lbl, e) => (lbl, e))).separatedBy(tk(',')) ~> (
        kvs => Map(kvs: _*)) ~
      tk('}')
  }

  def PrimOp: Rule1[E.PrimOp] = rule {
    tk('+') ~ push(E.PrimOp.add) |
      tk('-') ~ push(E.PrimOp.sub) |
      tk('*') ~ push(E.PrimOp.mul) |
      tk('/') ~ push(E.PrimOp.div)
  }

  def Expr5: Rule1[S.Expr] = rule {
    Variable ~> E.Var |
      Prim ~> E.Prim |
      Record ~> E.Record |
      (tk('(') ~ Expr ~ tk(')'))
  }

  def Expr4: Rule1[S.Expr] = rule {
    Expr5 ~
      zeroOrMore(Lookup) ~
      zeroOrMore(Argument ~> E.App)
  }

  def Expr3: Rule1[S.Expr] = rule {
    Expr4 ~
        zeroOrMore(
          tk('*') ~ Expr4 ~> ((l: S.Expr, r: S.Expr) => E.App(E.App(E.PrimOp.mul, l), r)) |
          tk('/') ~ Expr4 ~> ((l: S.Expr, r: S.Expr) => E.App(E.App(E.PrimOp.div, l), r)))
  }

  def Expr2: Rule1[S.Expr] = rule {
    Expr3 ~
        zeroOrMore(
          tk('+') ~ Expr3 ~> ((l: S.Expr, r: S.Expr) => E.App(E.App(E.PrimOp.add, l), r)) |
          tk('-') ~ Expr3 ~> ((l: S.Expr, r: S.Expr) => E.App(E.App(E.PrimOp.sub, l), r)))
  }

  def Expr1: Rule1[S.Expr] = rule {
    tk('\\') ~ oneOrMore(Variable) ~ tk("->") ~ Expr ~> ((vars, body) => vars.foldRight(body)(E.Lam)) |
    tk("if") ~ Expr ~ tk("then") ~ Expr ~ tk("else") ~ Expr ~> E.ITE |
    Expr2
  }

  def LetBindings: Rule1[Seq[(String, S.Expr)]] = rule {
    zeroOrMore(
      Reserved("let") ~ Variable ~ tk('=') ~ Expr ~> ((v, bound) => (v, bound)) ~ tk(';'))
  }

  def Expr0: Rule1[S.Expr] = rule {
    LetBindings ~ Expr1 ~> (
        (lets: Seq[(String, S.Expr)], body: S.Expr) =>
          lets.foldRight(body) { case ((v, bound), body) => E.Let(v, bound, body) })
  }

  def Expr: Rule1[S.Expr] = Expr0

  def ExprOnly: Rule1[S.Expr] = rule { WS ~ Expr ~ EOI }

  def Def: Rule1[(String, S.Expr)] = rule {
    Reserved("def") ~ Variable ~ tk('=') ~ Expr ~> ((v, body) => (v, body)) ~ tk(';')
  }

  def Package: Rule1[S.Package] = rule {
    WS ~ zeroOrMore(Def) ~> (defs => S.Package(Map(defs: _*))) ~ EOI
  }
}
