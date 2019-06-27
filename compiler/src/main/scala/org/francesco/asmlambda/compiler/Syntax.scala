package org.francesco.asmlambda.compiler

object Syntax {
  sealed trait SwitchCase

  sealed trait Scalar extends Expr
  object Scalar {
    case class I64(v: Long) extends Scalar with SwitchCase
    case class F64(v: Double) extends Scalar
    case class Text(v: String) extends Scalar with SwitchCase
    case class Bool(v: Boolean) extends Scalar
    case object Nil extends Scalar
  }

  sealed trait Arity
  object Arity {
    case class Fixed(n: Int) extends Arity
    case object Variadic extends Arity
  }

  sealed trait PrimOp extends Expr {
    val arity: Arity
  }

  object PrimOp {
    import Arity._

    case object Add extends PrimOp {
      val arity = Fixed(2)
    }
    case object Mul extends PrimOp {
      val arity = Fixed(2)
    }
    case object Sub extends PrimOp {
      val arity = Fixed(2)
    }
    case object Div extends PrimOp {
      val arity = Fixed(2)
    }
    case object Eq extends PrimOp {
      val arity = Fixed(2) // (Any, Any) -> Bool
    }
    case object LessEq extends PrimOp {
      val arity = Fixed(2)
    }
    case object Less extends PrimOp {
      val arity = Fixed(2)
    }
    case object Greater extends PrimOp {
      val arity = Fixed(2)
    }
    case object GreaterEq extends PrimOp {
      val arity = Fixed(2)
    }
    case object VectorGet extends PrimOp {
      val arity = Fixed(2) // (Vector, I64) -> Any
    }
    case object VectorLen extends PrimOp {
      val arity = Fixed(1) // (Vector) -> I64
    }
    case object VectorNew extends PrimOp {
      val arity = Variadic // (Any...) -> Vector
    }
    case object ToText extends PrimOp {
      val arity = Fixed(1) // (Any) -> Text
    }
    case object Not extends PrimOp {
      val arity = Fixed(1) // (Bool) -> Bool
    }
    case object Or extends PrimOp {
      val arity = Fixed(2) // (Bool, Bool) -> Bool
    }
    case object And extends PrimOp {
      val arity = Fixed(2) // (Bool, Bool) -> Bool
    }
    case object MapKeys extends PrimOp {
      val arity = Fixed(1) // (Map) -> Vector
    }
    case object MapGet extends PrimOp {
      val arity = Fixed(2) // (Map, I64) -> Any
    }
    case object MapPut extends PrimOp {
      val arity = Fixed(3) // (Map, Any, Any) -> Any
    }
    case object PairFst extends PrimOp {
      val arity = Fixed(1) // (Pair) -> Any
    }
    case object PairSnd extends PrimOp {
      val arity = Fixed(1) // (Pair) -> Any
    }
  }

  sealed trait Expr
  object Expr {
    case class Var(v: String) extends Expr with SwitchCase

    case class Map(fields: ImmArray[(Expr, Expr)]) extends Expr
    def mkMap(fields: (Expr, Expr)*): Expr = Map(ImmArray(fields: _*))

    case class Vector(elements: ImmArray[Expr]) extends Expr
    def mkVector(els: Expr*): Expr = Vector(ImmArray(els: _*))

    case class Pair(fst: Expr, snd: Expr) extends Expr

    case class Lam(args: ImmArray[String], body: Program) extends Expr

    case class App(fun: Expr, args: ImmArray[Expr]) extends Expr
    def mkApp(fun: Expr, args: Expr*): Expr = App(fun, ImmArray(args: _*))

    case class Switch(scrutined: Expr, cases: ImmArray[(SwitchCase, Expr)]) extends Expr

    case class ITE(scrutined: Expr, l: Expr, r: Option[Expr]) extends Expr

    case class Do(body: Program) extends Expr
  }

  sealed trait Form
  object Form {
    /** A group of (possibly) mutually recursive functions. All names _must_ be distinct. */
    case class Defs(defs: ImmArray[Def]) extends Form
    def mkDefs(defs: Def*): Defs = Defs(ImmArray(defs: _*))

    case class Let(v: String, bound: Syntax.Program) extends Form

    /** A naked expression */
    case class Expr(expr: Syntax.Expr) extends Form
  }

  case class Def(v: String, args: ImmArray[String], body: Program)

  /** if the program doesn't end with an expression Nil will be returned. */
  case class Program(forms: ImmArray[Form])
  def mkProgram(forms: Form*): Program = Program(ImmArray(forms: _*))
}
