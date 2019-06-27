package org.francesco.asmlambda.compiler

import Syntax._
import Syntax.{Expr => E}

/*
/** Renames so that there is no shadowing anywhere in the program. Also scope checks the program. */
object NoShadowing {
  private type Counters = Map[String, Int]

  private def varName(counters: Counters, v: String): String = {
    counters.get(v) match {
      case None => throw new RuntimeException(s"Out of scope variable $v!")
      case Some(counter) =>
        if (counter == 0) {
          v
        } else {
          v + "$" + counter.toString
        }
    }
  }

  private def bumpVar(counters: Counters, v: String): (Counters, String) = {
    val newCounters: Counters = counters.get(v) match {
      case None => counters + (v -> 0)
      case Some(counter) => counters + (v -> (counter + 1))
    }
    (newCounters, varName(newCounters, v))
  }

  private def telescope(
      counters: Counters,
      vars: ImmArray[String]): (Counters, ImmArray[String]) = {
    val (newCounters, reverseVars) = vars.foldLeft((counters, List.empty[String])) {
      case ((counters, varsSoFar), v) =>
        val (newCounters, newV) = bumpVar(counters, v)
        (newCounters, newV :: varsSoFar)
    }
    (newCounters, ImmArray(reverseVars.reverse: _*))
  }

  private def expr(counters: Counters, e0: Expr): (Counters, Expr) = e0 match {
    case E.Var(v) => (counters, E.Var(varName(counters, v)))
    case E.Map(flds) => (counters, E.Map(flds.map { case (lbl, e) => (lbl, expr(counters, e)._2) }))
    case E.Vector(els) => (counters, E.Vector(els.map(expr(counters, _)._2)))
    case E.Lam(vars, body) =>
      val (newCounters, newArgs) = telescope(counters, vars)
      (counters, E.Lam(newArgs, expr(newCounters, body)._2))
    case E.App(fun, args) =>
      (counters, E.App(expr(counters, fun)._2, args.map(expr(counters, _)._2)))
    case E.ITE(cond, l, r) =>
      (counters, E.ITE(expr(counters, cond)._2, expr(counters, l)._2, r.map(expr(counters, _)._2))
    case E.Switch(e, cases) =>
      (
        counters,
          E.Switch(
            expr(counters, e)._2,
            cases.map {
              case (i: Scalar.I64, mbBody) =>
                (i, mbBody.map(expr(counters, _)._2))
              case (txt: Scalar.Text, mbBody) =>
                (txt, mbBody.map(expr(counters, _)._2))
              case (E.Var(v), mbBody) =>
                val (newCounters, newV) = bumpVar(counters, v)
                (E.Var(newV), mbBody.map(expr(newCounters, _)._2))
            }
          )
      )
    case E.Do(p0) =>
      var newProgram: List[Expr] = List.empty
      for (e <- p0) {

      }
      E.Do(ImmArray(newProgram.reverse: _*))
    case pop: PrimOp => pop
    case scalar: Scalar => scalar
  }

  private def program(counters0: Counters, p0: Program): Program = {
    var newProgram: List[Form] = List.empty
    var counters = counters0
    for (form <- p0.forms) {
      form match {
        case Form.Expr(e) =>
          newProgram = Form.Expr(expr(counters, e)) :: newProgram
        case Form.Let(v, bound) =>
          val newBound = expr(counters, bound)
          val (newCounters, newV) = bumpVar(counters, v)
          newProgram = Form.Let(newV, newBound) :: newProgram
          counters = newCounters
        case Form.Defs(defs0) =>
          // Note that we _know_ that the names in `defs0` do not shadow each other, but they
          // might shadow previous names.
          var newCounters1 = counters
          val defs1 = defs0.map {
            case Def(v, args, program) =>
              var (newCounters2, newV) = bumpVar(newCounters1, v)
              newCounters1 = newCounters2
              Def(newV, args, program)
          }
          val defs2 = defs1.map {
            case Def(newV, args, p) =>
              val (newCounters2, newArgs) = telescope(newCounters1, args)
              Def(newV, newArgs, program(newCounters2, p))
          }
          newProgram = Form.Defs(defs2) :: newProgram
          counters = newCounters1
      }
    }
    Program(ImmArray(newProgram.reverse: _*))
  }

  def apply(p: Program): Program = program(Map.empty, p)
}
*/
