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
    "to-text" -> PrimOp.ToText,
    "not" -> PrimOp.Not,
    "or" -> PrimOp.Or,
    "and" -> PrimOp.And,
    "map-keys" -> PrimOp.MapKeys,
    "map-get" -> PrimOp.MapGet,
    "map-put" -> PrimOp.MapPut,
    "vec-new" -> PrimOp.VectorNew,
    "vec-get" -> PrimOp.VectorGet,
    "vec-len" -> PrimOp.VectorLen,
  )

  val reserved: Set[String] = Set("def", "let", "if", "fn", "switch", "fst", "snd")

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

    case Sexp.Scalar(scalar) => scalar

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

    case Sexp.Vector(els) =>
      val builder = IA.newBuilder[Expr]
      for (el <- els) {
        builder += expr(el)
      }
      Expr.Vector(builder.result())

    case Sexp.PairSeq() =>
      Scalar.Nil

    case Sexp.ListSeq(Sexp.Var("fn"), Sexp.Pair(args), body @ _*) =>
      Expr.Lam(checkArgsList(args), program(body.iterator))

    case Sexp.ListSeq(Sexp.Var("if"), cond, l) =>
      Expr.ITE(expr(cond), expr(l), None)

    case Sexp.ListSeq(Sexp.Var("if"), cond, l, r) =>
      Expr.ITE(expr(cond), expr(l), Some(expr(r)))

    case Sexp.ListSeq(Sexp.Var("switch"), e, scases @ _*) =>
      if (scases.length % 2 == 0) {
        val builder = IA.newBuilder[(SwitchCase, Expr)]
        for (i <- 0 until scases.length by 2) {
          val switchCase = scases(i) match {
            case ValidVar(v) => Expr.Var(v)
            case Sexp.Scalar(i: Scalar.I64) => i
            case Sexp.Scalar(txt: Scalar.Text) => txt
            case scase => throw ParseError(s"Bad switch case: $scase")
          }
          val body = expr(scases(i + 1))
          builder += ((switchCase, body))
        }
        Expr.Switch(expr(e), builder.result())
      } else {
        throw ParseError(s"Got odd number of element in switch cases expressions: $scases")
      }

    case Sexp.ListSeq(Sexp.Var("do"), body @ _*) =>
      Expr.Do(program(body.iterator))

    case Sexp.List(ImmArrayCons(fun, args)) =>
      Expr.App(expr(fun), args.map(expr))
  }

  def program(sexps: Iterator[Sexp]): Program = {
    val builder = IA.newBuilder[Form]

    val currentMutualDefs = IA.newBuilder[Def]
    val currentMutualNames = mutable.Set[String]()
    def pushMutual(): Unit = {
      if (currentMutualNames.nonEmpty) {
        builder += Form.Defs(currentMutualDefs.result())
        currentMutualDefs.clear()
        currentMutualNames.clear()
      }
    }

    for (sexp <- sexps) {
      sexp match {
        case Sexp.ListSeq(Sexp.Var("let"), Sexp.Var(v), body @ _*) =>
          pushMutual()
          builder += Form.Let(v, program(body.iterator))
        case Sexp.ListSeq(Sexp.Var("def"), Sexp.Var(v), Sexp.Pair(args), body @ _*) =>
          if (currentMutualNames.contains(v)) {
            throw ParseError(s"Duplicate name $v in def block")
          }
          currentMutualDefs += Def(v, checkArgsList(args), program(body.iterator))
          currentMutualNames += v
        case _ =>
          pushMutual()
          builder += Form.Expr(expr(sexp))
      }
    }
    pushMutual()

    Program(builder.result())
  }
}
