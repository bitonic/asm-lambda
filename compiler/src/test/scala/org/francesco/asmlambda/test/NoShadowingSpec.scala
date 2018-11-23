package org.francesco.asmlambda.test

import org.francesco.asmlambda.compiler.{ImmArray, Parser, Reader, Sexp}
import org.francesco.asmlambda.compiler.Syntax.Program
import org.francesco.asmlambda.compiler.NoShadowing

import scala.language.implicitConversions
import org.scalatest.{FreeSpec, Matchers, Assertion}

class NoShadowingSpec extends FreeSpec with Matchers {
  implicit def stringSexps(s: String): ImmArray[Sexp] = Reader(s, true)
  implicit def stringProgram(s: String): Program = Parser.program(stringSexps(s).iterator)

  case class ShouldBeProgram(p1: Program) {
    def shouldBeProgram(p2: Program): Assertion = p1 shouldBe p2
  }
  implicit def programShouldBeProgram(p: Program): ShouldBeProgram = ShouldBeProgram(p)

  "lambda" - {
    "simple" in {
      NoShadowing("(fn [x x] x)") shouldBeProgram "(fn [x x$1] x$1)"
    }

    "nested" in {
      NoShadowing("(fn [x y x] (+ x y) (fn [y x] (set! y x)))") shouldBeProgram
        "(fn [x y x$1] (+ x$1 y) (fn [y$1 x$2] (set! y$1 x$2)))"
    }
  }

  "switch" in {
    NoShadowing("""
      (let blah 7)
      (switch blah
        [42 "something"]
        [blah (to-text blah)])
    """) shouldBeProgram """
      (let blah 7)
      (switch blah
        [42 "something"]
        [blah$1 (to-text blah$1)])
    """
  }

  "lets" in {
    NoShadowing("""(let x "blah") x (let x 42) x""") shouldBeProgram """(let x "blah") x (let x$1 42) x$1"""
  }

  "defs" in {
    NoShadowing("""
      (def foo [] 42)
      (mutual
        (def bar [] (foo))
        (def foo [] (bar)))
    """) shouldBeProgram """
      (def foo [] 42)
      (mutual
        (def bar [] (foo$1))
        (def foo$1 [] (bar)))
    """
  }
}
