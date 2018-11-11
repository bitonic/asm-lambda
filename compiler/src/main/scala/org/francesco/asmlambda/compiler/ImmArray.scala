package org.francesco.asmlambda.compiler

import scala.collection.mutable
import scala.collection.JavaConverters._

/** An immutable array. Simple wrapper around [[scala.collection.mutable.ArraySeq]] with utilities to slice, cons, snoc.
  */
class ImmArray[+A] private (array: mutable.ArraySeq[A]) {
  def slice(from: Int, until: Int): ImmArray[A] = new ImmArray(array.slice(from, until))

  def length: Int = array.length

  def isEmpty: Boolean = array.isEmpty

  def head: A = array.head

  def tail: ImmArray[A] = new ImmArray(array.slice(1, length))

  def iterator: Iterator[A] = array.iterator

  def toSeq: Seq[A] = array

  def map[B](f: A => B): ImmArray[B] = new ImmArray(array.map(f))

  def collect[B](f: PartialFunction[A, B]): ImmArray[B] = new ImmArray(array.collect(f))

  def apply(ix: Int): A = array(ix)

  override def equals(obj: Any): Boolean = obj match {
    case arr: ImmArray[A] => iterator sameElements arr.iterator
    case _ => false
  }

  override def toString: String = {
    val stringBuilder = new mutable.StringBuilder()
    stringBuilder ++= "ImmArray("
    for (i <- 0 until length) {
      stringBuilder ++= array(i).toString
      if (i < length - 1) {
        stringBuilder ++= ", "
      }
    }
    stringBuilder ++= ")"
    stringBuilder.mkString
  }

  override def hashCode(): Int = array.hashCode()
}

object ImmArray {
  private val emptySingleton: ImmArray[Nothing] = new ImmArray[Nothing](mutable.ArraySeq())

  def empty[A]: ImmArray[A] = emptySingleton

  def apply[A](els: A*): ImmArray[A] = new ImmArray(mutable.ArraySeq(els: _*))

  def unapply[A](arg: ImmArray[A]): Boolean = arg.isEmpty

  object Cons {
    def unapply[A](arg: ImmArray[A]): Option[(A, ImmArray[A])] = if (arg.isEmpty) {
      None
    } else {
      Option((arg.head, arg.tail))
    }
  }

  type Builder[A] = mutable.Builder[A, ImmArray[A]]

  def newBuilder[A]: Builder[A] = {
    val elems: java.util.ArrayList[A] = new java.util.ArrayList[A]()

    new Builder[A] {
      override def +=(elem: A): this.type = {
        elems.add(elem)
        this
      }
      override def clear(): Unit = elems.clear()
      override def result(): ImmArray[A] = ImmArray(elems.asScala: _*)

      override def sizeHint(size: Int): Unit = elems.ensureCapacity(elems.size + size)
    }
  }
}
