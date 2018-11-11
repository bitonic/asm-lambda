package org.francesco.asmlambda.compiler

import org.francesco.asmlambda.compiler.{ImmArray => IA}
import Syntax._

case class ParseError(msg: String) extends Throwable(msg)

object Parser {
  val reserved: Set[String] = Set("def", "let", "if", "fn", "do", "switch", "set!")

  private def checkArgsList(args: ImmArray[Sexp]): ImmArray[String] = {
    val varArgs = args.collect {
      case Sexp.Var(arg) => arg
    }
    if (varArgs.length != args.length) {
      throw ParseError(s"Bad argument list: $args")
    }
    varArgs
  }

  private object ValidVar {
    def unapply(sexp: Sexp): Option[String] = sexp match {
      case Sexp.Var(v) if !reserved.contains(v) => Some(v)
      case _ => None
    }
  }

  def expr(sexp: Sexp): Expr = sexp match {
    case ValidVar(v) => Expr.Var(v)

    case Sexp.Scalar(scalar) => Expr.Scalar(scalar)

    case Sexp.ListSeq(Sexp.Var("set!"), Sexp.Var(v), body) => Expr.Set(v, expr(body))

    case Sexp.Map(els) =>
      if (els.length % 2 == 0) {
        val builder = IA.newBuilder[(Expr, Expr)]
        for (i <- 0 until els.length by 2) {
          builder += ((expr(els(i)), expr(els(i+1))))
        }
        Expr.Map(builder.result())
      } else {
        throw ParseError(s"Got odd numbers of elements in map expression: $els")
      }

    case Sexp.Vector(els) => Expr.Vector(els.map(expr))

    case Sexp.ListSeq(Sexp.Var("fn"), Sexp.Vector(args), body @ _*) =>
      Expr.Lam(checkArgsList(args), program(body.iterator))

    case Sexp.ListSeq(Sexp.Var("if"), cond, l) =>
      Expr.ITE(expr(cond), expr(l), None)

    case Sexp.ListSeq(Sexp.Var("switch"), e, scases @ _*) =>
      val cases: ImmArray[(SwitchCase, Program)] = ImmArray(scases: _*).map {
        case Sexp.VectorSeq(ValidVar(v), body @ _*) => (Expr.Var(v), program(body.iterator))
        case Sexp.VectorSeq(Sexp.Scalar(sym: Scalar.Symbol), body @ _*) => (sym, program(body.iterator))
        case Sexp.VectorSeq(Sexp.Scalar(i: Scalar.I64), body @ _*) => (i, program(body.iterator))
        case scase => throw ParseError(s"Bad switch case: $scase")
      }
      Expr.Switch(expr(e), cases)

    case Sexp.ListSeq(Sexp.Var("do"), body @ _*) =>
      Expr.Do(program(body.iterator))

    case Sexp.List(IA.Cons(fun, args)) =>
      Expr.App(expr(fun), args.map(expr))

    case Sexp.List(ImmArray()) =>
      Expr.Scalar(Scalar.Nil)
  }

  def program(sexps: Iterator[Sexp]): Program = {
    val builder = IA.newBuilder[Form]
    var inDefs = false // are we in a defs block?
    val defBuilder = IA.newBuilder[Def]

    def pushDef(defn: Def): Unit = {
      if (!inDefs) {
        inDefs = true
      }
      defBuilder += defn
    }

    def clearDefs(): Unit = if (inDefs) {
      builder += Form.Defs(defBuilder.result())
      defBuilder.clear()
      inDefs = false
    }

    for (sexp <- sexps) {
      sexp match {
        case Sexp.ListSeq(Sexp.Var("let"), Sexp.Var(v), body @ _*) =>
          clearDefs()
          builder += Form.Let(v, program(body.iterator))
        case Sexp.ListSeq(Sexp.Var("def"), Sexp.Var(v), Sexp.Vector(args), body @ _*) =>
          pushDef(Def(v, checkArgsList(args), program(body.iterator)))
        case _ =>
          clearDefs()
          builder += Form.Expr(expr(sexp))
      }
    }

    clearDefs()
    Program(builder.result())
  }
}