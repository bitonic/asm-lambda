package org.francesco.asmlambda.compiler

import org.francesco.asmlambda.compiler.{ImmArray => IA}
import Syntax._

import scala.collection.mutable

case class ParseError(msg: String) extends Throwable(msg)

object Parser {
  val primOps: Map[String, PrimOp] = Map(
    "+" -> PrimOp.Add,
    "*" -> PrimOp.Mul,
    "-" -> PrimOp.Sub,
    "/" -> PrimOp.Div,
    "=" -> PrimOp.Eq,
    "<=" -> PrimOp.LessEq,
    "<" -> PrimOp.Less,
    ">" -> PrimOp.Greater,
    ">=" -> PrimOp.GreaterEq,
    "vector-get" -> PrimOp.VectorGet,
    "vector-set!" -> PrimOp.VectorSet,
    "vector-len" -> PrimOp.VectorLen,
    "vector-slice" -> PrimOp.VectorSlice,
    "vector-new" -> PrimOp.VectorNew,
    "to-text" -> PrimOp.ToText,
    "not" -> PrimOp.Not,
    "or" -> PrimOp.Or,
    "and" -> PrimOp.And,
    "map-keys" -> PrimOp.MapKeys,
    "map-get" -> PrimOp.MapGet,
    "map-put!" -> PrimOp.MapPut,
    "copy" -> PrimOp.Copy,
  )

  val reserved: Set[String] = Set("def", "let", "if", "fn", "do", "switch", "set!", "mutual")

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
    case ValidVar(v) =>
      primOps.get(v) match {
        case None => Expr.Var(v)
        case Some(pop) => pop
      }

    case Sexp.Scalar(scalar) => Expr.Scalar(scalar)

    case Sexp.ListSeq(Sexp.Var("set!"), Sexp.Var(v), body) => Expr.Set(v, expr(body))

    case Sexp.Map(els) =>
      if (els.length % 2 == 0) {
        val builder = IA.newBuilder[(Expr, Expr)]
        for (i <- 0 until els.length by 2) {
          builder += ((expr(els(i)), expr(els(i + 1))))
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
      val cases: ImmArray[(SwitchCase, Option[Expr])] = ImmArray(scases: _*).map {
        case Sexp.VectorSeq(ValidVar(v)) => (Expr.Var(v), None)
        case Sexp.VectorSeq(ValidVar(v), body) => (Expr.Var(v), Some(expr(body)))
        case Sexp.VectorSeq(Sexp.Scalar(sym: Scalar.Symbol)) => (sym, None)
        case Sexp.VectorSeq(Sexp.Scalar(sym: Scalar.Symbol), body) => (sym, Some(expr(body)))
        case Sexp.VectorSeq(Sexp.Scalar(i: Scalar.I64)) => (i, None)
        case Sexp.VectorSeq(Sexp.Scalar(i: Scalar.I64), body) => (i, Some(expr(body)))
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

  def mutual(sexps: Iterator[Sexp]): ImmArray[Def] = {
    val builder = IA.newBuilder[Def]
    val names = mutable.Set[String]()

    for (sexp <- sexps) {
      sexp match {
        case Sexp.ListSeq(Sexp.Var("def"), Sexp.Var(v), Sexp.Vector(args), body @ _*) =>
          if (names.contains(v)) {
            throw ParseError(s"Duplicate name $v in mutual block")
          }
          builder += Def(v, checkArgsList(args), program(body.iterator))
        case _ => ParseError(s"Unexpected non-def s-exp in mutual block")
      }
    }

    builder.result()
  }

  def program(sexps: Iterator[Sexp]): Program = {
    val builder = IA.newBuilder[Form]

    for (sexp <- sexps) {
      sexp match {
        case Sexp.ListSeq(Sexp.Var("let"), Sexp.Var(v), body) =>
          builder += Form.Let(v, expr(body))
        case Sexp.ListSeq(Sexp.Var("def"), Sexp.Var(v), Sexp.Vector(args), body @ _*) =>
          builder += Form.Defs(ImmArray(Def(v, checkArgsList(args), program(body.iterator))))
        case Sexp.ListSeq(Sexp.Var("mutual"), body @ _*) =>
          builder += Form.Defs(mutual(body.iterator))
        case _ =>
          builder += Form.Expr(expr(sexp))
      }
    }

    Program(builder.result())
  }
}
