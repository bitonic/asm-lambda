package org.francesco.asmlambda.compiler

import org.francesco.asmlambda.runtime.{Record, Array}

import scala.language.implicitConversions

case class Value(getValue: Object)

object Value {
  implicit def i64LongValue(v: Long): Value = Value(java.lang.Long.valueOf(v))
  implicit def i64IntValue(v: Int): Value = Value(java.lang.Long.valueOf(v.toLong))
  implicit def f64Value(v: Double): Value = Value(java.lang.Double.valueOf(v))
  implicit def booleanValue(v: Boolean): Value = Value(java.lang.Boolean.valueOf(v))
  implicit def stringValue(s: java.lang.String): Value = Value(s)
  def Record(els: (String, Value)*): Value = {
    val elements = new java.util.HashMap[String, Object](els.size)
    for (el <- els) {
      elements.put(el._1, el._2.getValue)
    }
    Value(new Record(elements))
  }
  def Array(els: Value*): Value = {
    Value(new Array(els.map(_.getValue).toArray))
  }
}
