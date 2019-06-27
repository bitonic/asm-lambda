package org.francesco.asmlambda.test

import org.francesco.asmlambda.runtime.StringHAMT
import org.scalatest.{FreeSpec, Matchers}

class StringHAMTSpec extends FreeSpec with Matchers {
  "put" - {
    "one put" in {
      var map = new StringHAMT()
      map = map.put("1-key", "1-val")
      map.get("1-key") shouldBe "1-val"
      map.keys().toSet shouldBe Set("1-key")
    }

    "two puts" in {
      var map = new StringHAMT()
      map = map.put("1-key", "1-val")
      map = map.put("2-key", "2-val")
      map.get("1-key") shouldBe "1-val"
      map.get("2-key") shouldBe "2-val"
      map.keys().toSet shouldBe Set("1-key", "2-key")
    }

    "three puts" in {
      var map = new StringHAMT()
      map = map.put("1-key", "1-val")
      map.get("2-key") shouldBe null
      map.get("3-key") shouldBe null
      map = map.put("2-key", "2-val")
      map.get("3-key") shouldBe null
      map = map.put("3-key", "3-val")
      map.get("1-key") shouldBe "1-val"
      map.get("2-key") shouldBe "2-val"
      map.get("3-key") shouldBe "3-val"
      map.keys().toSet shouldBe Set("1-key", "2-key", "3-key")
    }

    "1000 puts and remove" in {
      var map = new StringHAMT()
      for (i <- 1 to 1000) {
        map = map.put(s"$i-key", s"$i-val")
        map.get(s"$i-key") shouldBe s"$i-val"
      }
      map.keys().toSet shouldBe Set((1 to 1000).map{ i => s"$i-key" }: _*)
      map.size() shouldBe 1000
      for (i <- 1000 to 1 by -1) {
        map = map.remove(s"$i-key")
        map.get(s"$i-key") shouldBe null
      }
      map.keys().toSet shouldBe Set()
      map.size() shouldBe 0
    }
  }
}
