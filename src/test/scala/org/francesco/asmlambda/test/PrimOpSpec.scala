package org.francesco.asmlambda.test

import org.scalatest._
import org.francesco.asmlambda.PrimOp._
import org.francesco.asmlambda.Value._

class PrimOpSpec extends FreeSpec with Matchers {
  "toText" - {
    "I64" in {
      toText(42.getValue) shouldBe "42"
    }

    "F64" in {
      toText(42.0.getValue) shouldBe "42.0"
    }

    "String" in {
      toText("blah\"baz".getValue) shouldBe """"blah\"baz""""
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
        toText(Record("foo" -> Record("bar" -> 1, "baz" -> 2), "blah" -> true).getValue) shouldBe
          "{blah = true, foo = {bar = 1, baz = 2}}"
      }
    }
  }
}
