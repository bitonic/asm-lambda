package org.francesco.asmlambda.compiler

import scala.collection.mutable.ArraySeq

object Syntax {
  sealed trait SwitchCase

  sealed trait Scalar
  object Scalar {
    case class I64(v: Long) extends Scalar with SwitchCase
    case class F64(v: Double) extends Scalar
    case class Text(v: String) extends Scalar
    case class Bool(v: Boolean) extends Scalar
    case object Nil extends Scalar
    case class Symbol(sym: String) extends Scalar with SwitchCase
  }

  sealed trait PrimOp extends Expr {
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
      val arity = 2 // (Any, Any) -> Bool
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
    case object VectorGet extends PrimOp {
      val arity = 2 // (Vector, I64) -> Any
    }
    case object VectorSet extends PrimOp {
      val arity = 3 // (Vector, I64, Any) -> Unit
    }
    case object VectorLen extends PrimOp {
      val arity = 1 // (Vector) -> I64
    }
    case object VectorSlice extends PrimOp {
      val arity = 3 // (Vector, I64, I64) -> Vector
    }
    case object VectorNew extends PrimOp {
      val arity = 2 // (I64) -> Vector
    }
    case object ToText extends PrimOp {
      val arity = 1 // (Any) -> Text
    }
    case object Not extends PrimOp {
      val arity = 1 // (Bool) -> Bool
    }
    case object Or extends PrimOp {
      val arity = 2 // (Bool, Bool) -> Bool
    }
    case object And extends PrimOp {
      val arity = 2 // (Bool, Bool) -> Bool
    }
    case object MapKeys extends PrimOp {
      val arity = 1 // (Map) -> Vector
    }
    case object MapGet extends PrimOp {
      val arity = 2 // (Map, I64) -> Any
    }
    case object MapPut extends PrimOp {
      val arity = 3 // (Map, Any, Any) -> Any
    }
    case object Copy extends PrimOp {
      val arity = 1 // (Any) -> Any
    }
    case object SetKeys extends PrimOp {
      val arity = 1 // (Set) -> Vector
    }
    case object SetAdd extends PrimOp {
      val arity = 1 // (Set, Any) -> Uni
    }
  }

  sealed trait Expr
  object Expr {
    case class Var(v: String) extends Expr

    case class Map(fields: Predef.Map[String, Expr]) extends Expr
    def mkMap(fields: (String, Expr)*): Expr = Map(Predef.Map(fields: _*))

    case class Vector(elements: ArraySeq[Expr]) extends Expr
    def mkVector(els: Expr*): Expr = Vector(ArraySeq(els: _*))

    case class Lam(args: ArraySeq[String], body: Expr) extends Expr

    case class App(fun: Expr, args: ArraySeq[Expr]) extends Expr
    def mkApp(fun: Expr, args: Expr*): Expr = App(fun, ArraySeq(args: _*))

    case class ITE(cond: Expr, left: Expr, right: Option[Expr]) extends Expr

    case class Switch(scrutined: Expr, cases: ArraySeq[(SwitchCase, Expr)], default: Option[Expr]) extends Expr
  }

  sealed trait Form
  object Form {
    /** A group of (possibly) mutually recursive functions */
    case class Defs(defs: ArraySeq[Def]) extends Form
    case class Let(v: String, bound: Program) extends Form
    /** A naked expression */
    case class Expr(expr: Syntax.Expr) extends Form
  }

  case class Def(args: ArraySeq[Expr], body: Program)

  /** if the program doesn't end with an expression Unit will be returned. */
  case class Program(forms: ArraySeq[Form])
}
