package org.francesco.asmlambda.test

import org.scalatest._

import org.francesco.asmlambda.compiler.Value._
import org.francesco.asmlambda.runtime.PrimOp.toText

class PrimOpSpec extends FreeSpec with Matchers {
  "toText" - {
    "I64" in {
      toText(42.getValue) shouldBe "42"
    }

    "F64" in {
      toText(42.0.getValue) shouldBe "42.0"
    }

    "Text" in {
      toText("BEGIN \" \\ \n \t \u0000 \uDCAB \u0012 END".getValue) shouldBe
          "\"BEGIN \" \\\\ \\n \\t \\u0000 \\uDCAB \\u0012 END\""
    }

    "record" - {
      "0" in {
        toText(Record().getValue) shouldBe "{}"
      }

      "1" in {
        toText(Record("foo" -> true).getValue) shouldBe "{foo = true}"
      }

      "2" in {
        toText(Record("foo" -> 42, "1" -> 3.14).getValue) shouldBe "{1 = 3.14, foo = 42}"
      }

      "nested" in {
        toText(Record("foo" -> Array(1, Record("bar" -> 1, "baz" -> 2), 2), "blah" -> true).getValue) shouldBe
          "{blah = true, foo = #[1, {bar = 1, baz = 2}, 2]}"
      }
    }

    "array" - {
      "0" in {
        toText(Array().getValue) shouldBe "#[]"
      }

      "1" in {
        toText(Array(1).getValue) shouldBe "#[1]"
      }

      "2" in {
        toText(Array(1, 2).getValue) shouldBe "#[1, 2]"
      }
    }
  }
}
