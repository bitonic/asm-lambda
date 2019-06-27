package org.francesco.asmlambda.test

import org.francesco.asmlambda.compiler.{ImmArray, Parser, Reader, Sexp}
import org.francesco.asmlambda.compiler.Syntax._
import org.francesco.asmlambda.compiler.{ImmArray => IA}
import org.francesco.asmlambda.compiler.ValueOps._

import scala.language.implicitConversions
import org.scalatest.{FreeSpec, Matchers, Assertion}

class ParserSpec extends FreeSpec with Matchers {
  implicit def stringSexps(s: String): ImmArray[Sexp] = Reader(s)
  implicit def stringSexp(s: String): Sexp = {
    val sexps = Reader(s)
    if (sexps.length == 1) {
      sexps(0)
    } else {
      throw new RuntimeException(s"Expected 1 sexps, but got ${sexps.length}")
    }
  }

  case class ShouldBeExpr(sexp: Sexp) {
    def shouldBeExpr(e: Expr): Assertion = Parser.expr(sexp) shouldBe e
  }
  implicit def stringShouldBeExpr(s: String): ShouldBeExpr = {
    ShouldBeExpr(s)
  }

  case class ShouldBeProgram(sexps: ImmArray[Sexp]) {
    def shouldBeProgram(p: Program): Assertion = Parser.program(sexps.iterator) shouldBe p
  }
  implicit def stringShouldBeProgram(s: String): ShouldBeProgram = ShouldBeProgram(s)

  import org.francesco.asmlambda.compiler.ExprOps._

  "scalars" - {
    "i64" in {
      "42" shouldBeExpr 42
    }

    "f64" in {
      "44.55" shouldBeExpr 44.55
    }

    "bool" in {
      "#[true false]" shouldBeExpr vec(true, false)
    }

    "nil" in {
      "[]" shouldBeExpr nil
    }

    "string" in {
      """ "test" """ shouldBeExpr str("test")
    }
  }

  "primop" - {
    "plus" in {
      "+" shouldBeExpr PrimOp.Add
    }
  }

  "compound" - {
    "map (0)" in {
      "{}" shouldBeExpr map()
    }

    "map (1)" in {
      "{one two}" shouldBeExpr map(("one", "two"))
    }

    "map (2)" in {
      "{one two, three four}" shouldBeExpr map(("one", "two"), ("three", "four"))
    }

    "vec (0)" in {
      "#[]" shouldBeExpr vec()
    }

    "vec (1)" in {
      "#[one]" shouldBeExpr vec("one")
    }

    "vec (2)" in {
      "#[one two]" shouldBeExpr vec("one", "two")
    }

    "mixed" in {
      val data =
        """
          {
            1 #[foo true []],
            #[something] {something-else 42},
          }
        """
      data shouldBeExpr
        map(
          (1, vec("foo", true, nil)),
          (vec("something"), map(("something-else", 42)))
        )
    }
  }

  "lambda" - {
    "no args, no body" in {
      "(fn [])" shouldBeExpr lam(IA())
    }

    "no args, body" in {
      "(fn [] one two three)" shouldBeExpr lam(IA(), "one", "two", "three")
    }

    "3 args" in {
      """(fn [a b c] "blaaaa")""" shouldBeExpr lam(IA("a", "b", "c"), str("blaaaa"))
    }
  }

  "switch" in {
    val e =
      """
        (switch some-expr
          123 [] ; nothing
          v (do da da da))
      """
    e shouldBeExpr
        switch(
          "some-expr",
          caseI64(123, nil),
          caseVar("v", `do`("da", "da", "da")))
  }

  "if" - {
    "no else case" in {
      """(if cond then)""" shouldBeExpr `if`("cond", "then")
    }

    "with else case" in {
      """(if cond then else)""" shouldBeExpr `if`("cond", "then", Some("else"))
    }
  }

  "application" - {
    "0 args" in {
      """(f)""" shouldBeExpr app("f")
    }

    "1 arg" in {
      """(f x)""" shouldBeExpr app("f", "x")
    }

    "2 args" in {
      """(f x y)""" shouldBeExpr app("f", "x", "y")
    }
  }

  "program" in {
    """
      (let x)
      (+ 1 2)
      (let y y-body-1)
      (def f [] f-step-1 f-step-2)
      (let z z-body-1 z-body-2)
      (def g [a])
      (def h [a b] h-step-1)
      "Hello World!"
    """ shouldBeProgram mkProgram(
      let("x"),
      Form.Expr(app(PrimOp.Add, 1, 2)),
      let("y", "y-body-1"),
      defs(
        `def`("f", IA(), "f-step-1", "f-step-2")
      ),
      let("z", "z-body-1", "z-body-2"),
      defs(
        `def`("g", IA("a")),
        `def`("h", IA("a", "b"), "h-step-1"),
      ),
      Form.Expr(str("Hello World!")),
    )
  }
}
