package org.francesco.asmlambda.compiler

import org.francesco.asmlambda.compiler.Syntax.Scalar

import scala.collection.mutable

private class LambdaLift {
  import LambdaLift._

  val generatedDefnCounters: mutable.Map[String, Int] = mutable.Map.empty
  val definitions: mutable.Map[String, Def] = mutable.Map.empty

  def newDefinitionName(defName: String): String = {
    val count = generatedDefnCounters.getOrElse(defName, 0)
    generatedDefnCounters += (defName -> (count + 1))
    defName + "$" + count.toString
  }

  /** Returns the new expression and the free variables therein */
  def expr(containingDefName: String, names: Names, e0: Syntax.Expr): (Expr, Set[String]) =
    e0 match {
      case scalar: Syntax.Scalar => (Expr.Scalar(scalar), Set.empty)
      case Syntax.Expr.ITE(cond, l, mbR) =>
        val (newCond, condVars) = expr(containingDefName, names, cond)
        val (newL, lVars) = expr(containingDefName, names, l)
        val (newR, rVars) = mbR match {
          case None => (None, Set.empty[String])
          case Some(r) =>
            val (newR, rVars) = expr(containingDefName, names, r)
            (Some(newR), rVars)
        }
        (Expr.ITE(newCond, newL, newR), condVars ++ lVars ++ rVars)
      case Syntax.Expr.Map(fields) =>
        val newFields = ImmArray.newBuilder[(Expr, Expr)]
        val vars = mutable.Set[String]()
        for (field <- fields) {
          val (newK, kVars) = expr(containingDefName, names, field._1)
          val (newV, vVars) = expr(containingDefName, names, field._2)
          newFields += (newK -> newV)
          vars ++= kVars
          vars ++= vVars
        }
        (Expr.Map(newFields.result()), vars.toSet)
      case Syntax.Expr.Vector(elements) =>
        val newElements = ImmArray.newBuilder[Expr]
        val vars = mutable.Set[String]()
        for (element <- elements) {
          val (newElement, elementVars) = expr(containingDefName, names, element)
          newElements += newElement
          vars ++= elementVars
        }
        (Expr.Vector(newElements.result()), vars.toSet)
      case Syntax.Expr.Var(v) =>
        names(v) match {
          case Name.Var => (Expr.Var(v), Set(v))
          case Name.Def(fullDefName, capturedArgs, argsArity) =>
            (Expr.DefRef(fullDefName, capturedArgs, argsArity), Set.empty)
        }
      case Syntax.Expr.Set(v, e) =>
        val (newE, eVars) = expr(containingDefName, names, e)
        (Expr.Set(v, newE), eVars + v)
      case Syntax.Expr.Lam(args, body) =>
        // create a new definition with on the fly with all the free variables of the body as captured
        // arg
        val bodyNames = Map(args.map(arg => (arg, Name.Var)).toSeq: _*)
        val (newBody, bodyFreeVars0) = program(containingDefName, names ++ bodyNames, body)
        val bodyFreeVars = bodyFreeVars0 &~ Set(args.toSeq: _*)
        val capturedArgs = ImmArray(bodyFreeVars.toSeq: _*)
        val defName = newDefinitionName(containingDefName)
        definitions += (defName -> Def(capturedArgs, args, newBody))
        (Expr.DefRef(defName, capturedArgs, args.length), bodyFreeVars)
      case Syntax.Expr.App(fun, args) =>
        val (newArgsReversed, argsFreeVars) = args.foldLeft((List.empty[Expr], Set.empty[String])) {
          case ((newArgs, argsFreeVars), arg) =>
            val (newArg, argFreeVars) = expr(containingDefName, names, arg)
            (newArg :: newArgs, argsFreeVars ++ argFreeVars)
        }
        val newArgs = ImmArray(newArgsReversed.reverse: _*)
        lazy val default = {
          val (newFun, funFreeVars) = expr(containingDefName, names, fun)
          (Expr.DynamicCall(newFun, newArgs), argsFreeVars ++ funFreeVars)
        }
        fun match {
          case pop: Syntax.PrimOp => if (args.length == pop.arity) {
            (Expr.PrimOpCall(pop, newArgs), argsFreeVars)
          } else {
            throw new RuntimeException(s"Got ${args.length} arguments for prim op $pop, while I expected ${pop.arity}")
          }
          case Syntax.Expr.Var(v) => names(v) match {
            case Name.Def(defName, capturedArgs, argsArity) => if (args.length == argsArity) {
              (Expr.StaticCall(defName, capturedArgs, newArgs), argsFreeVars)
            } else {
              throw new RuntimeException(s"Got ${args.length} arguments for def $defName, while I expected $argsArity")
            }
            case Name.Var => default
          }
          case _ => default
      }
      case pop: Syntax.PrimOp => throw new RuntimeException(s"Got naked primop $pop")
      case Syntax.Expr.Do(p) =>
        val (newP, pVars) = program(containingDefName, names, p)
        (Expr.Do(newP), pVars)
    }

  def program(containingDefName: String, names: Names, p0: Syntax.Program): (Block, Set[String]) =
    ???
}

object LambdaLift {
  sealed trait SwitchCases
  case class Int64Cases(cases: ImmArray[(Scalar.I64, Expr)])
  case class SymbolCases(cases: ImmArray[(Scalar.Symbol, Expr)])
  case class StringCases(cases: ImmArray[(Scalar.Text, Expr)])

  sealed trait Expr
  object Expr {

    /** at this stage, this is for let- and lam-references only.
      * references to definitions (local or global) get turned into
      * [[DefRef]] or [[StaticCall]].
      */
    case class Var(v: String) extends Expr
    case class Set(v: String, e: Expr) extends Expr

    /** reference to a def (originally local or global). note that we
      * still need this beyond [[StaticCall]] because people might
      * reference them as functions.
      */
    case class DefRef(defName: String, capturedArgs: ImmArray[String], argsArity: Int) extends Expr
    case class Scalar(s: Syntax.Scalar) extends Expr
    case class Map(fields: ImmArray[(Expr, Expr)]) extends Expr
    case class Vector(elements: ImmArray[Expr]) extends Expr
    case class StaticCall(defName: String, capturedArgs: ImmArray[String], args: ImmArray[Expr])
        extends Expr
    case class DynamicCall(fun: Expr, args: ImmArray[Expr]) extends Expr
    /** The args must be equal to the arity of the pop */
    case class PrimOpCall(pop: Syntax.PrimOp, args: ImmArray[Expr]) extends Expr
    case class ITE(cond: Expr, l: Expr, r: Option[Expr]) extends Expr
    case class Switch(scrutined: Expr, cases: SwitchCases, default: Expr) extends Expr
    case class Let(v: String, bound: Expr) extends Expr
    case class Do(body: Block) extends Expr
  }

  type Block = ImmArray[Expr]

  case class Def(capturedArgs: ImmArray[String], args: ImmArray[String], body: Block)

  case class Program(defs: Map[String, Def], body: Block)

  private sealed trait Name
  private object Name {
    /** this name derives from a let- or lam-bound variable */
    case object Var extends Name
    /** this name comes from a definition, with the given captured args applied. */
    case class Def(fullDefName: String, capturedArgs: ImmArray[String], argsArity: Int) extends Name
  }
  private type Names = Map[String, Name]
}
