package org.francesco.asmlambda.compiler

import scala.collection.mutable.ArraySeq

object Syntax {
  sealed trait Prim
  object Prim {
    case class I64(v: Long) extends Prim
    case class F64(v: Double) extends Prim
    case class Text(v: String) extends Prim
    case class Bool(v: Boolean) extends Prim
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
  }

  sealed trait Expr
  object Expr {
    case class Var(v: String) extends Expr
    case class Record(fields: Map[String, Expr]) extends Expr
    case class Lookup(rec: Expr, field: String) extends Expr
    case class Update(rec: Expr, field: String, body: Expr) extends Expr
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
    }
    case class ITE(cond: Expr, left: Expr, right: Expr) extends Expr
    case class Let(v: String, bound: Expr, body: Expr) extends Expr
    case class Def(defName: String, bound: Definition, body: Expr) extends Expr
  }

  case class Definition(args: ArraySeq[String], body: Expr)

  case class Package(defs: Map[String, Definition], body: Expr)

}