package org.francesco.asmlambda.compiler

import scala.collection.mutable.ArraySeq

object Syntax {
  sealed trait Prim
  object Prim {
    case class I64(v: Long) extends Prim
    case class F64(v: Double) extends Prim
    case class Text(v: String) extends Prim
    case class Bool(v: Boolean) extends Prim
    case object Nil extends Prim
  }

  sealed trait PrimOp {
    val arity: Int
  }
  object PrimOp {
    case object Add extends PrimOp {
      val arity = 2
    }
    case object Mul extends PrimOp {
      val arity = 2
    }
    case object Sub extends PrimOp {
      val arity = 2
    }
    case object Div extends PrimOp {
      val arity = 2
    }
    case object Eq extends PrimOp {
      val arity = 2
    }
    case object LessEq extends PrimOp {
      val arity = 2
    }
    case object Less extends PrimOp {
      val arity = 2
    }
    case object Greater extends PrimOp {
      val arity = 2
    }
    case object GreaterEq extends PrimOp {
      val arity = 2
    }
    case object ArrayGet extends PrimOp {
      val arity = 2 // array, index
    }
    case object ArrayLen extends PrimOp {
      val arity = 1 // array
    }
    case object ToText extends PrimOp {
      val arity = 1 // any
    }
    case object Or extends PrimOp {
      val arity = 2 // bool, bool
    }
    case object Car extends PrimOp {
      val arity = 1 // cons
    }
    case object Cdr extends PrimOp {
      val arity = 1 // cons
    }
  }

  sealed trait Expr
  object Expr {
    case class Var(v: String) extends Expr
    case class Record(fields: Map[String, Expr]) extends Expr
    def mkRecord(fields: (String, Expr)*): Expr = Record(Map(fields: _*))
    case class RecordLookup(rec: Expr, field: String) extends Expr
    case class RecordUpdate(rec: Expr, field: String, body: Expr) extends Expr
    case class Array(elements: ArraySeq[Expr]) extends Expr
    def mkArray(els: Expr*): Expr = Array(ArraySeq(els: _*))
    case class Lam(args: ArraySeq[String], body: Expr) extends Expr
    case class App(fun: Expr, args: ArraySeq[Expr]) extends Expr
    def mkApp(fun: Expr, args: Expr*): Expr = App(fun, ArraySeq(args: _*))
    case class Prim(prim: Syntax.Prim) extends Expr
    case class PrimOp(pop: Syntax.PrimOp) extends Expr
    object PrimOp {
      val add = PrimOp(Syntax.PrimOp.Add)
      val sub = PrimOp(Syntax.PrimOp.Sub)
      val mul = PrimOp(Syntax.PrimOp.Mul)
      val div = PrimOp(Syntax.PrimOp.Div)
      val eq = PrimOp(Syntax.PrimOp.Eq)
      val less = PrimOp(Syntax.PrimOp.Less)
      val lessEq = PrimOp(Syntax.PrimOp.LessEq)
      val greater = PrimOp(Syntax.PrimOp.Greater)
      val greaterEq = PrimOp(Syntax.PrimOp.GreaterEq)
      val arrGet = PrimOp(Syntax.PrimOp.ArrayGet)
      val arrLen = PrimOp(Syntax.PrimOp.ArrayLen)
      val toText = PrimOp(Syntax.PrimOp.ToText)
      val or = PrimOp(Syntax.PrimOp.Or)
      val car = PrimOp(Syntax.PrimOp.Car)
      val cdr = PrimOp(Syntax.PrimOp.Cdr)
    }
    case class ITE(cond: Expr, left: Expr, right: Expr) extends Expr
    case class Let(v: String, bound: Expr, body: Expr) extends Expr
    case class Def(defName: String, bound: Definition, body: Expr) extends Expr
    case class Cons(car: Expr, cdr: Expr) extends Expr
    def mkList(els: Expr*): Expr = els.foldRight[Expr](Prim(Syntax.Prim.Nil)){ case (car, cdr) => Cons(car, cdr) }
  }

  case class Definition(args: ArraySeq[String], body: Expr)

  case class Package(defs: Map[String, Definition], body: Expr)

}
