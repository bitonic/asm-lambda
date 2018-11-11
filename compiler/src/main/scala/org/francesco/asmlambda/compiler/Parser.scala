package org.francesco.asmlambda.compiler

import scala.collection.mutable.ArraySeq
import Syntax._

import scala.annotation.tailrec

case class ReaderError(msg: String) extends Throwable(msg)

object Parser {

  /** Consumes as many definitions as it finds and returns them along with the unprocessed sexps.
    */
  @tailrec
  private def defs(processed: List[Def], remaining: List[Sexp]): (List[Def], List[Sexp]) = ???

  @tailrec
  private def expr(sexp: Sexp): Expr = ???

  private def defn(name: String, args: ArraySeq[String], body: ArraySeq[Sexp]) = ???

  def apply(sexps: ArraySeq[Sexp]): Program = {
    @tailrec
    def program(processed: List[Form], remaining: List[Sexp]): Program = remaining match {
      case Nil => Program(ArraySeq(processed.reverse: _*))
      case MatchDef() =>
        val (defs, newRest) = defs(List.empty, remaining)
        program(Form.Defs(defs) :: processed, newRest)
      case MatchLet(v, sbound) :: rest =>
        val bound = restartProgram(sbound)
        program(Form.Let(v, bound) :: processed, rest)
      case sexp :: rest =>
        program(Form.Expr(expr(sexp)) :: processed, rest)
    }

    def restartProgram(sexps: List[Sexp]): Program = program(List.empty, sexps)

    program(List.empty, List(sexps: _*))
  }

  object MatchDef {
    def unapply(arg: Sexp): Option[(String, ArraySeq[String], ArraySeq[Sexp])] = ???
  }

  object MatchLet {
    def unapply(arg: Sexp): Option[(String, List[Sexp])] = ???
  }
}
