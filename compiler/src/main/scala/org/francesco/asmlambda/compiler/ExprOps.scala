package org.francesco.asmlambda.compiler

import scala.language.implicitConversions

import Syntax._

object ExprOps {
  implicit def intExpr(v: Int): Expr = Expr.Scalar(Scalar.I64(v.toLong))
  implicit def longExpr(v: Long): Expr = Expr.Scalar(Scalar.I64(v))
  implicit def doubleExpr(v: Double): Expr = Expr.Scalar(Scalar.F64(v))
  implicit def booleanExpr(b: Boolean): Expr = Expr.Scalar(Scalar.Bool(b))
  implicit def stringExpr(s: String): Expr = Expr.Var(s)

  def map(els: (Expr, Expr)*): Expr = Expr.Map(ImmArray(els: _*))
  def vec(els: Expr*): Expr = Expr.Vector(ImmArray(els: _*))

  val nil: Expr = Expr.Scalar(Scalar.Nil)
  def str(s: String): Expr = Expr.Scalar(Scalar.Text(s))
  def sym(s: String): Expr = Expr.Scalar(Scalar.Symbol(s))
  def `set!`(v: String, e: Expr): Expr = Expr.Set(v, e)
  def lam(args: ImmArray[String], body: Form*): Expr = Expr.Lam(args, Program(ImmArray(body: _*)))
  def switch(e: Expr, cases: (SwitchCase, Program)*) = Expr.Switch(e, ImmArray(cases: _*))
  def caseVar(v: String, body: Form*): (SwitchCase, Program) = (Expr.Var(v), Program(ImmArray(body: _*)))
  def caseSym(v: String, body: Form*): (SwitchCase, Program) = (Scalar.Symbol(v), Program(ImmArray(body: _*)))
  def caseI64(i: Long, body: Form*): (SwitchCase, Program) = (Scalar.I64(i), Program(ImmArray(body: _*)))
  def `do`(body: Form*): Expr = Expr.Do(Program(ImmArray(body: _*)))

  def expr(e: Expr): Form = Form.Expr(e)
}
