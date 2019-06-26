package org.francesco.asmlambda.test

import org.scalatest.{FreeSpec, Matchers}

import scala.language.implicitConversions
import org.francesco.asmlambda.compiler._
import org.francesco.asmlambda.runtime.Value

class SexpSpec extends FreeSpec with Matchers {
  implicit def intSexp(v: Int): Sexp = Sexp.Scalar(Syntax.Scalar.I64(v.toLong))
  implicit def longSexp(v: Long): Sexp = Sexp.Scalar(Syntax.Scalar.I64(v))
  implicit def doubleSexp(v: Double): Sexp = Sexp.Scalar(Syntax.Scalar.F64(v))
  implicit def booleanSexp(b: Boolean): Sexp = Sexp.Scalar(Syntax.Scalar.Bool(b))
  implicit def stringSexp(s: String): Sexp = Sexp.Var(s)
  def txt(txt: String): Sexp = Sexp.Scalar(Syntax.Scalar.Text(txt))
  val nil: Sexp = Sexp.Scalar(Syntax.Scalar.Nil)

  def map(els: Sexp*): Sexp = Sexp.Seq(SeqKind.Map, ImmArray(els: _*))
  def list(els: Sexp*): Sexp = Sexp.Seq(SeqKind.List, ImmArray(els: _*))
  def vec(els: Sexp*): Sexp = Sexp.Seq(SeqKind.Vector, ImmArray(els: _*))
  def pair(els: Sexp*): Sexp = Sexp.Seq(SeqKind.Pair, ImmArray(els: _*))

  def sexps(xs: Sexp*): ImmArray[Sexp] = ImmArray(xs: _*)

  "i64" - {
    "normal" in {
      Reader("1234") shouldBe sexps(1234)
    }

    "signed +" in {
      Reader("+1234") shouldBe sexps(1234)
    }

    "signed -" in {
      Reader("-1234") shouldBe sexps(-1234)
    }

    "multiple" in {
      Reader("345 -2342 +2322 84") shouldBe sexps(345, -2342, 2322, 84)
    }
  }

  "f64" - {
    "normal" in {
      Reader("7457.769") shouldBe sexps(7457.769)
    }

    "signed +" in {
      Reader("+1234.44") shouldBe sexps(1234.44)
    }

    "signed -" in {
      Reader("-1234.77") shouldBe sexps(-1234.77)
    }

    "multiple" in {
      Reader("345.0 -2342.55 +2322.1 84.567") shouldBe sexps(345.0, -2342.55, 2322.1, 84.567)
    }

    "scientific" in {
      Reader("13.879e5") shouldBe sexps(13.879e5)
    }

    "nothing after dot" in {
      Reader("1.") shouldBe sexps(1.0) // not so great, consequence of using toDouble
    }
  }

  "var" - {
    "+ -" in {
      Reader("+ +test - -abc") shouldBe sexps("+", "+test", "-", "-abc")
    }

    "simple" in {
      Reader("abc") shouldBe sexps("abc")
    }

    "almost scalars" in {
      Reader("nill truee falsee") shouldBe sexps("nill", "truee", "falsee")
    }

    "leading digit" in {
      Reader("1-one") shouldBe sexps("1-one")
    }

    "with comment" in {
      Reader("one;two") shouldBe sexps("one")
    }
  }

  "booleans" in {
    Reader("true false") shouldBe sexps(true, false)
  }

  "nil" in {
    Reader("<>") shouldBe sexps(pair())
  }

  "lists" in {
    Reader("(a [b c] d {e [f (g)]}) (simpler-list) ()") shouldBe
      sexps(
        list("a", vec("b", "c"), "d", map("e", vec("f", list("g")))),
        list("simpler-list"),
        list())
  }

  "strings" - {
    "simple" in {
      Reader(""" "foo" """) shouldBe sexps(txt("foo"))
    }

    "escaped" in {
      Reader("\"BEGIN \\\" \\\\ \\n \\t \\u0000 \\uDCAB \\u0012 END\"") shouldBe
          sexps(txt("BEGIN \" \\ \n \t \u0000 \uDCAB \u0012 END"))
    }

    "unicode" in {
      val sampleTxt =
        """
          Mathematics and sciences:

            ∮ E⋅da = Q,  n → ∞, ∑ f(i) = ∏ g(i),      ⎧⎡⎛┌─────┐⎞⎤⎫
                                                      ⎪⎢⎜│a²+b³ ⎟⎥⎪
            ∀x∈ℝ: ⌈x⌉ = −⌊−x⌋, α ∧ ¬β = ¬(¬α ∨ β),    ⎪⎢⎜│───── ⎟⎥⎪
                                                      ⎪⎢⎜⎷ c₈   ⎟⎥⎪
            ℕ ⊆ ℕ₀ ⊂ ℤ ⊂ ℚ ⊂ ℝ ⊂ ℂ,                   ⎨⎢⎜       ⎟⎥⎬
                                                      ⎪⎢⎜ ∞     ⎟⎥⎪
            ⊥ < a ≠ b ≡ c ≤ d ≪ ⊤ ⇒ (⟦A⟧ ⇔ ⟪B⟫),      ⎪⎢⎜ ⎲     ⎟⎥⎪
                                                      ⎪⎢⎜ ⎳aⁱ-bⁱ⎟⎥⎪
            2H₂ + O₂ ⇌ 2H₂O, R = 4.7 kΩ, ⌀ 200 mm     ⎩⎣⎝i=1    ⎠⎦⎭

          Linguistics and dictionaries:

            ði ıntəˈnæʃənəl fəˈnɛtık əsoʊsiˈeıʃn
            Y [ˈʏpsilɔn], Yen [jɛn], Yoga [ˈjoːgɑ]

          APL:

            ((V⍳V)=⍳⍴V)/V←,V    ⌷←⍳→⍴∆∇⊃‾⍎⍕⌈

          Nicer typography in plain text files:

            ╔══════════════════════════════════════════╗
            ║                                          ║
            ║   • ‘single’ and “double” quotes         ║
            ║                                          ║
            ║   • Curly apostrophes: “We’ve been here” ║
            ║                                          ║
            ║   • Latin-1 apostrophe and accents: '´`  ║
            ║                                          ║
            ║   • ‚deutsche‘ „Anführungszeichen“       ║
            ║                                          ║
            ║   • †, ‡, ‰, •, 3–4, —, −5/+5, ™, …      ║
            ║                                          ║
            ║   • ASCII safety test: 1lI|, 0OD, 8B     ║
            ║                      ╭─────────╮         ║
            ║   • the euro symbol: │ 14.95 € │         ║
            ║                      ╰─────────╯         ║
            ╚══════════════════════════════════════════╝

          Combining characters:

            STARGΛ̊TE SG-1, a = v̇ = r̈, a⃑ ⊥ b⃑

          Greek (in Polytonic):

            The Greek anthem:

            Σὲ γνωρίζω ἀπὸ τὴν κόψη
            τοῦ σπαθιοῦ τὴν τρομερή,
            σὲ γνωρίζω ἀπὸ τὴν ὄψη
            ποὺ μὲ βία μετράει τὴ γῆ.

          ! ~
        """
      val stringBuilder = new java.lang.StringBuilder()
      Value.escapeString(stringBuilder, sampleTxt)
      val escaped = stringBuilder.toString
      Reader(escaped) shouldBe sexps(txt(sampleTxt))
    }
  }
}
