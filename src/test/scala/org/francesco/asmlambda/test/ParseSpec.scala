package org.francesco.asmlambda.test

import org.scalatest._
import scala.collection.mutable.ArraySeq

import org.francesco.asmlambda.{Parser, Syntax => S}
import org.francesco.asmlambda.Syntax.Expr
import org.francesco.asmlambda.Syntax.{Expr => E}
import org.francesco.asmlambda.Syntax.{Prim => P}

import scala.language.implicitConversions

class ParseSpec extends FreeSpec with Matchers {
  implicit def varStr(s: String): Expr = E.Var(s)
  implicit def primBool(b: Boolean): Expr = E.Prim(P.Bool(b))
  implicit def primLongI64(i: Long): Expr = E.Prim(P.I64(i))
  implicit def primIntI64(i: Int): Expr = E.Prim(P.I64(i.toLong))
  implicit def primF64(i: Double): Expr = E.Prim(P.F64(i))

  def assertParse[A](s: String, p: fastparse.P[_] => fastparse.P[A]): A = {
    fastparse.parse(s, p) match {
      case fastparse.Parsed.Success(x, _) => x
      case fastparse.Parsed.Failure(_, _, extra) =>
        sys.error(s"Parse error: ${extra.trace().longAggregateMsg}")
    }
  }

  def parseExpr(s: String): Expr = assertParse(s, Parser.exprOnly(_))

  def parsePackage(s: String): S.Package = assertParse(s, Parser.`package`(_))

  "literal (I64)" in {
    parseExpr("4") shouldBe E.Prim(P.I64(4))
  }

  "literal (F64)" in {
    parseExpr("4.0") shouldBe E.Prim(P.F64(4.0))
  }

  "record" in {
    parseExpr("{this = true, that = false}") shouldBe E.Record(
      Map("this" -> true, "that" -> false))
  }

  "lookup (var head)" in {
    parseExpr("x.foo") shouldBe E.Lookup("x", "foo")
  }

  "lookup (record head)" in {
    parseExpr("{x = true}.x") shouldBe E.Lookup(E.Record(Map("x" -> true)), "x")
  }

  "update (var)" in {
    parseExpr("x{foo = bar, blah = baz}") shouldBe E.Update(E.Update("x", "foo", "bar"), "blah", "baz")
  }

  "update (app)" in {
    parseExpr("a(b{foo = bar})") shouldBe E.mkApp("a", E.Update("b", "foo", "bar"))
  }

  "application (all vars)" in {
    parseExpr("f(x, y)") shouldBe E.mkApp("f", "x", "y")
  }

  "lambda" in {
    parseExpr("""\(x, y, z) -> x(z, y(z))""") shouldBe
      E.Lam(ArraySeq("x", "y", "z"), E.mkApp("x", "z", E.mkApp("y", "z")))
  }

  "lambda (empty)" in {
    parseExpr("""\() -> 42""") shouldBe
      E.Lam(ArraySeq.empty, 42)
  }

  "lambda (app)" in {
    parseExpr("""(\(x) -> x)(\(y) -> y)""") shouldBe
      E.mkApp(E.Lam(ArraySeq("x"), "x"), E.Lam(ArraySeq("y"), "y"))
  }

  "lambda (in let)" in {
    parseExpr("""let id = \(x) -> x; id(42)""") shouldBe
      E.Let("id", E.Lam(ArraySeq("x"), "x"), E.mkApp("id", 42))
  }

  "lets" in {
    parseExpr("""
        let x = 1;
        let y = 2;
        x + y
      """) shouldBe
      E.Let(
        "x", 1,
        E.Let("y", 2, E.mkApp(E.PrimOp.add, "x", "y")))
  }

  "ITE" in {
    parseExpr("""
        if a(b, c)
          then {x = 3}
          else {y = true}
      """) shouldBe
      E.ITE(
        E.mkApp("a", "b", "c"),
        E.Record(Map("x" -> E.Prim(P.I64(3)))),
        E.Record(Map("y" -> E.Prim(P.Bool(true)))))
  }

  "ITE (simple)" in {
    parseExpr("""if true then 1 else 2""") shouldBe E.ITE(true, 1, 2)
  }

  "defs" in {
    parseExpr("""
        def foo(x, y) = x + y;
        def bar(x, y) = \(z) -> x + y + z;
        foo
      """) shouldBe
      E.Def(
        "foo",
        S.Definition(ArraySeq("x", "y"), E.mkApp(E.PrimOp.add, "x", "y")),
        E.Def(
          "bar",
          S.Definition(
            ArraySeq("x", "y"),
            E.Lam(ArraySeq("z"), E.mkApp(E.PrimOp.add, E.mkApp(E.PrimOp.add, "x", "y"), "z"))),
          "foo"
        )
      )
  }

  "defs & lets" in {
    parseExpr("""
        def foo(x, y) = x + y;
        let bar = 42;
        foo
      """) shouldBe
        E.Def(
          "foo",
          S.Definition(ArraySeq("x", "y"), E.mkApp(E.PrimOp.add, "x", "y")),
          E.Let(
            "bar",
            E.Prim(S.Prim.I64(42)),
            "foo"
          )
        )
  }

  "Package" in {
    parsePackage("""
        def foo (x, y, z) = {x = x, y = y, z = z};
        def bar () = 1 + 2;
        42
      """) shouldBe
      S.Package(
        Map(
          "foo" -> S
            .Definition(ArraySeq("x", "y", "z"), E.Record(Map("x" -> "x", "y" -> "y", "z" -> "z"))),
          "bar" -> S.Definition(
            ArraySeq.empty,
            E.mkApp(E.PrimOp.add, 1, 2))
        ),
        42)
  }

  "arithmetic" in {
    parseExpr("a * b + c / d - e * f") shouldBe
      E.mkApp(
        E.PrimOp.sub,
        E.mkApp(E.PrimOp.add, E.mkApp(E.PrimOp.mul, "a", "b"), E.mkApp(E.PrimOp.div, "c", "d")),
        E.mkApp(E.PrimOp.mul, "e", "f"))
  }

  "arithmetic in lambda" in {
    parseExpr("""\(a, b, c, d, e, f) -> a * b + c / d - e * f""") shouldBe
      E.Lam(ArraySeq("a", "b", "c", "d", "e", "f"),
        E.mkApp(
          E.PrimOp.sub,
          E.mkApp(E.PrimOp.add, E.mkApp(E.PrimOp.mul, "a", "b"), E.mkApp(E.PrimOp.div, "c", "d")),
          E.mkApp(E.PrimOp.mul, "e", "f")))
  }

}
