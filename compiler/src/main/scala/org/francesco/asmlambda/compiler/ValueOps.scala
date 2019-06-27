package org.francesco.asmlambda.compiler

import org.francesco.asmlambda.runtime.{Value, Pair}
import scala.language.implicitConversions

// To use value implicits easily
case class WrappedValue(value: Object)

object ValueOps {
  implicit def intValue(v: Int): WrappedValue = WrappedValue(java.lang.Long.valueOf(v))
  implicit def longValue(v: Long): WrappedValue = WrappedValue(java.lang.Long.valueOf(v))
  implicit def doubleValue(v: Double): WrappedValue = WrappedValue(java.lang.Double.valueOf(v))
  implicit def booleanValue(b: Boolean): WrappedValue = WrappedValue(java.lang.Boolean.valueOf(b))
  implicit def stringValue(s: String): WrappedValue = WrappedValue(s)
  implicit def arrayValue(v: Array[WrappedValue]): WrappedValue = WrappedValue(v.map(_.value))
  implicit def mapValue(v: Map[String, WrappedValue]): WrappedValue = {
    var map = Value.mapNew()
    for (entry <- v) {
      map = Value.mapPut(map, entry._1, entry._2.value)
    }
    WrappedValue(map)
  }
  implicit def pairValue(fst: WrappedValue, snd: WrappedValue): WrappedValue =
    WrappedValue(new Pair(fst.value, snd.value))

  def map(els: (String, WrappedValue)*): Map[String, WrappedValue] = Map(els: _*)
  def vec(els: WrappedValue*): Array[WrappedValue] = Array(els: _*)

  val nil: WrappedValue = WrappedValue(null)
}
