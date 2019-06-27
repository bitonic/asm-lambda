package org.francesco.asmlambda.test

import org.scalatest._
import org.francesco.asmlambda.runtime.Value
import org.francesco.asmlambda.compiler.ValueOps._
import org.francesco.asmlambda.compiler.WrappedValue

class ValueSpec extends FreeSpec with Matchers {
  "toText" - {
    def toText(v: WrappedValue): String = Value.toText(v.value).asInstanceOf[String]

    "I64" in {
      toText(42) shouldBe "42"
    }

    "F64" in {
      toText(42.0) shouldBe "42.0"
    }

    "Text" in {
      toText("BEGIN \" \\ \n \t \u0000 \uDCAB \u0012 END") shouldBe
          "\"BEGIN \\\" \\\\ \\n \\t \\u0000 \\uDCAB \\u0012 END\""
    }

    /*
    "Map" - {
      "0" in {
        toText(map()) shouldBe "{}"
      }

      "1" in {
        toText(map(("foo", true))) shouldBe """{"foo" true}"""
      }

      "2" in {
        toText(map(("foo", 42), ("1", 3.14))) shouldBe """{"1" 3.14, "foo" 42}"""
      }

      "nested" in {
        toText(
          map(("42 false", vec(1, map(("bar", 1), ("baz", 2)), 2)), ("blah", vec(nil, 56.65)))) shouldBe
          """{"42 false" [1 {"bar" 1, "baz" 2} 2], "blah" [() 56.65]}"""
      }
    }
    */

    "vector" - {
      "0" in {
        toText(vec()) shouldBe "#[]"
      }

      "1" in {
        toText(vec(1)) shouldBe "#[1]"
      }

      "2" in {
        toText(vec(1, 2)) shouldBe "#[1 2]"
      }
    }
  }
}
