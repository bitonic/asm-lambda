package org.francesco.asmlambda.test

import org.scalatest._
import org.francesco.asmlambda.Parser
import org.francesco.asmlambda.{Syntax => S}
import org.francesco.asmlambda.Syntax.Expr
import org.francesco.asmlambda.Syntax.{Expr => E}
import org.francesco.asmlambda.Syntax.{Prim => P}
import org.parboiled2.ParseError
import scala.util.Try
import scala.language.implicitConversions

class ParseSpec extends FlatSpec with Matchers {
  implicit def varStr(s: String): Expr = E.Var(s)

  def assertParse[A](s: String, f: Parser => Try[A]): A = {
    val parser = new Parser(s)
    f(parser).recover {
      case err: ParseError => sys.error(s"Parse error: ${parser.formatError(err)}")
    }.get
  }

  def parseExpr(s: String): Expr = assertParse(s, _.ExprOnly.run())

  def parsePackage(s: String): S.Package = assertParse(s, _.Package.run())

  it should "literal" in {
    parseExpr("4") shouldBe E.Prim(P.Int64(4))
  }

  it should "record" in {
    parseExpr("{this = true, that = false}") shouldBe E.Record(
      Map("this" -> E.Prim(P.Bool(true)), "that" -> E.Prim(P.Bool(false))))
  }

  it should "lookup (var head)" in {
    parseExpr("x.foo") shouldBe E.Lookup("x", "foo")
  }

  it should "lookup (record head)" in {
    parseExpr("{x = true}.x") shouldBe E.Lookup(E.Record(Map("x" -> E.Prim(P.Bool(true)))), "x")
  }

  it should "application (all vars)" in {
    parseExpr("f x y") shouldBe E.App(E.App("f", "x"), "y")
  }

  it should "lambda" in {
    parseExpr("""\x y z -> x z (y z) """) shouldBe
      E.Lam("x", E.Lam("y", E.Lam("z", E.App(E.App("x", "z"), E.App("y", "z")))))
  }

  it should "lets" in {
    parseExpr("""
        let x = 1;
        let y = 2;
        x + y
      """) shouldBe
      E.Let(
        "x",
        E.Prim(P.Int64(1)),
        E.Let("y", E.Prim(P.Int64(2)), E.App(E.App(E.PrimOp.add, "x"), "y")))
  }

  it should "ITE" in {
    parseExpr("""
        if a b c
          then {x = 3}
          else {y = true}
      """) shouldBe
      E.ITE(
        E.App(E.App("a", "b"), "c"),
        E.Record(Map("x" -> E.Prim(P.Int64(3)))),
        E.Record(Map("y" -> E.Prim(P.Bool(true)))))
  }

  it should "Package" in {
    parsePackage("""
        def foo = \x y z -> {x = x, y = y, z = z};
        def bar = 1 + 2;
      """.stripMargin) shouldBe
      S.Package(
        Map(
          "foo" -> E
            .Lam("x", E.Lam("y", E.Lam("z", E.Record(Map("x" -> "x", "y" -> "y", "z" -> "z"))))),
          "bar" -> E.App(E.App(E.PrimOp.add, E.Prim(P.Int64(1))), E.Prim(P.Int64(2)))
        ))
  }

  it should "arithmetic" in {
    parseExpr("a * b + c / d - e * f") shouldBe
        E.mkApp(
          E.PrimOp.sub,
          E.mkApp(
            E.PrimOp.add,
            E.mkApp(E.PrimOp.mul, "a", "b"),
            E.mkApp(E.PrimOp.div, "c", "d")),
          E.mkApp(E.PrimOp.mul, "e", "f"))
  }
}
