package org.francesco.asmlambda.test

import org.scalatest.{FreeSpec, Matchers}

import scala.collection.mutable.ArraySeq
import scala.language.implicitConversions
import scala.collection.JavaConverters._
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
  def sym(v: String): Sexp = Sexp.Scalar(Syntax.Scalar.Symbol(v))

  def set(els: Sexp*): Sexp = Sexp.List(ListKind.Set, els.asJava)
  def map(els: Sexp*): Sexp = Sexp.List(ListKind.Map, els.asJava)
  def list(els: Sexp*): Sexp = Sexp.List(ListKind.Normal, els.asJava)
  def vec(els: Sexp*): Sexp = Sexp.List(ListKind.Vector, els.asJava)

  def sexps(xs: Sexp*): ArraySeq[Sexp] = ArraySeq(xs: _*)

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

    "nothing after dot" in {
      the [ParseError] thrownBy Reader("1.") should have message "Unexpected end of input, expected digit"
    }
  }

  "var" - {
    "+ -" - {
      Reader("+ +test - -abc") shouldBe sexps("+", "+test", "-", "-abc")
      ()
    }

    "simple" - {
      Reader("abc") shouldBe sexps("abc")
      ()
    }

    "almost scalars" - {
      Reader("nill truee falsee") shouldBe sexps("nill", "truee", "falsee")
      ()
    }
  }

  "booleans" in {
    Reader("true false") shouldBe sexps(true, false)
  }

  "nil" in {
    Reader("nil") shouldBe sexps(nil)
  }

  "lists" in {
    Reader("(a [b c] d {e #{f (g)}}) (simpler-list) ()") shouldBe
      sexps(
        list("a", vec("b", "c"), "d", map("e", set("f", list("g")))),
        list("simpler-list"),
        list())
  }

  "symbol" - {
    "simple" in {
      Reader(":foo") shouldBe sexps(sym("foo"))
      ()
    }

    "not-scalars" in {
      Reader(":true :false :nil") shouldBe sexps(sym("true"), sym("false"), sym("nil"))
      ()
    }

    "only colon" in {
      the [ParseError] thrownBy Reader(":") should have message "Unexpected end of input, expected initial identifier character"
    }
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
