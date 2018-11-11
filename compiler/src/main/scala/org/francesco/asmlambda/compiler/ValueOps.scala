package org.francesco.asmlambda.compiler

import org.francesco.asmlambda.runtime.Value
import org.francesco.asmlambda.runtime.WrappedValue

import scala.language.implicitConversions

object ValueOps {
  implicit def intValue(v: Int): WrappedValue = new WrappedValue(v.toLong)
  implicit def longValue(v: Long): WrappedValue = new WrappedValue(v)
  implicit def doubleValue(v: Double): WrappedValue = new WrappedValue(v)
  implicit def booleanValue(b: Boolean): WrappedValue = new WrappedValue(b)
  implicit def stringValue(s: String): WrappedValue = new WrappedValue(s)
  implicit def arrayValue(v: Array[WrappedValue]): WrappedValue = new WrappedValue(v.map(_.value))
  implicit def mapValue(v: Map[WrappedValue, WrappedValue]): WrappedValue = {
    val map = Value.mapNew()
    for (entry <- v) {
      Value.mapPut(map, entry._1.value, entry._2.value)
    }
    new WrappedValue(map)
  }

  def map(els: (WrappedValue, WrappedValue)*): Map[WrappedValue, WrappedValue] = Map(els: _*)
  def vec(els: WrappedValue*): Array[WrappedValue] = Array(els: _*)

  val nil: WrappedValue = new WrappedValue(null)
}
