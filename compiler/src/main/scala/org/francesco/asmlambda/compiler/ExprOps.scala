package org.francesco.asmlambda.compiler

import scala.language.implicitConversions

import Syntax._

object ExprOps {
  /*
  implicit def intExpr(v: Int): Expr = Scalar.I64(v.toLong)
  implicit def longExpr(v: Long): Expr = Scalar.I64(v)
  implicit def doubleExpr(v: Double): Expr = Scalar.F64(v)
  implicit def booleanExpr(b: Boolean): Expr = Scalar.Bool(b)
  implicit def stringExpr(s: String): Expr = Expr.Var(s)

  def map(els: (Expr, Expr)*): Expr = Expr.Map(ImmArray(els: _*))
  // def vec(els: Expr*): Expr = Expr.Vector(ImmArray(els: _*))

  val nil: Expr = Scalar.Nil
  def str(s: String): Expr = Scalar.Text(s)
  def lam(args: ImmArray[String], body: Form*): Expr = Expr.Lam(args, Program(ImmArray(body: _*)))
  def switch(e: Expr, cases: (SwitchCase, Option[Expr])*) = Expr.Switch(e, ImmArray(cases: _*))
  def caseVar(v: String, body: Option[Expr] = None): (SwitchCase, Option[Expr]) =
    (Expr.Var(v), body)
  def caseI64(i: Long, body: Option[Expr] = None): (SwitchCase, Option[Expr]) =
    (Scalar.I64(i), body)
  def app(fun: Expr, args: Expr*): Expr = Expr.App(fun, ImmArray(args: _*))

  def expr(e: Expr): Form = Form.Expr(e)
   */
}
