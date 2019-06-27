package org.francesco.asmlambda.compiler

import scala.language.implicitConversions

import Syntax._

object ExprOps {
  implicit def intExpr(v: Int): Expr = Scalar.I64(v.toLong)
  implicit def longExpr(v: Long): Expr = Scalar.I64(v)
  implicit def doubleExpr(v: Double): Expr = Scalar.F64(v)
  implicit def booleanExpr(b: Boolean): Expr = Scalar.Bool(b)
  implicit def stringExpr(s: String): Expr = Expr.Var(s)

  implicit def exprForm(e: Expr): Form = Form.Expr(e)
  implicit def intForm(v: Int): Form = Scalar.I64(v.toLong)
  implicit def longForm(v: Long): Form = Scalar.I64(v)
  implicit def doubleForm(v: Double): Form = Scalar.F64(v)
  implicit def booleanForm(b: Boolean): Form = Scalar.Bool(b)
  implicit def stringForm(s: String): Form = Expr.Var(s)

  def map(els: (Expr, Expr)*): Expr = Expr.Map(ImmArray(els: _*))
  def vec(els: Expr*): Expr = Expr.Vector(ImmArray(els: _*))

  val nil: Expr = Scalar.Nil
  def str(s: String): Expr = Scalar.Text(s)
  def lam(args: ImmArray[String], body: Form*): Expr = Expr.Lam(args, Program(ImmArray(body: _*)))
  def switch(e: Expr, cases: (SwitchCase, Expr)*) = Expr.Switch(e, ImmArray(cases: _*))
  def caseVar(v: String, body: Expr): (SwitchCase, Expr) =
    (Expr.Var(v), body)
  def caseI64(i: Long, body: Expr): (SwitchCase, Expr) =
    (Scalar.I64(i), body)
  def caseString(txt: String, body: Expr): (SwitchCase, Expr) =
    (Scalar.Text(txt), body)
  def app(fun: Expr, args: Expr*): Expr = Expr.App(fun, ImmArray(args: _*))
  def pair(fst: Expr, snd: Expr): Expr = Expr.Pair(fst, snd)
  def `do`(body: Form*): Expr = Expr.Do(Program(ImmArray(body: _*)))
  def `if`(cond: Expr, `then`: Expr, `else`: Option[Expr] = None) = Expr.ITE(cond, `then`, `else`)

  def `def`(v: String, args: ImmArray[String], body: Form*) = Def(v, args, mkProgram(body: _*))

  def let(v: String, body: Form*) = Form.Let(v, mkProgram(body: _*))
  def defs(dfs: Def*) = Form.Defs(ImmArray(dfs: _*))
}
