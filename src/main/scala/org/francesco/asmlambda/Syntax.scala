package org.francesco.asmlambda

object Syntax {
  sealed trait Prim
  object Prim {
    case class Int64(x: Long) extends Prim
    case class Text(txt: String) extends Prim
    case class Bool(b: Boolean) extends Prim
  }

  sealed trait Expr
  object Expr {
    case class Var(v: String) extends Expr
    case class Record(fields: Map[String, Expr]) extends Expr
    case class Lookup(rec: Expr, field: String) extends Expr
    case class Lam(arg: String, body: Expr) extends Expr
    case class App(fun: Expr, arg: Expr) extends Expr
    def mkApp(head: Expr, args: Expr*): Expr =
      args.foldLeft(head)((fun, arg) => App(fun, arg))
    case class Prim(prim: Syntax.Prim) extends Expr
    case class PrimOp(pop: Int) extends Expr
    object PrimOp {
      val add: PrimOp = PrimOp(0)
      val sub: PrimOp = PrimOp(1)
      val mul: PrimOp = PrimOp(2)
      val div: PrimOp = PrimOp(3)
    }
    case class ITE(cond: Expr, left: Expr, right: Expr) extends Expr
    case class Let(v: String, bound: Expr, body: Expr) extends Expr
  }

  case class Package(defs: Map[String, Expr])

}
