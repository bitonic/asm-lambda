package org.francesco.asmlambda

/*
import org.objectweb.asm.{ClassWriter, Opcodes}

trait Evaluator {
  type Function
  type Package

  def evalPackage(pkg: Syntax.Package): Package
  def evalExpr(pkg: Package, expr: Syntax.Expr): Value[Function]
}

sealed trait Value[Fun] {
  case class Record[Fun](map: Map[String, Value[Fun]]) extends Value[Fun]
  case class Function[Fun](fun: Fun) extends Value[Fun]
  case class Prim[Fun](prim: Syntax.Prim) extends Value[Fun]
}

class SimpleEvaluator extends Evaluator {
  override type Function = Syntax.Expr
  override type Package = Map[String, Syntax.Expr]

  override def evalPackage(pkg: Syntax.Package): Map[String, Syntax.Expr] = pkg.defs
  override def evalExpr(
      pkg: Map[String, Syntax.Expr],
      expr: Syntax.Expr): Value[Syntax.Expr] = ???
}

class JvmEvaluator extends Evaluator {
  override type Function = Function1[Value[Function], Value[Function]]
  override type Package = Map[String, Function0[Value[Function]]]

  override def evalPackage(pkg: Syntax.Package): Package = ???
  override def evalExpr(
      pkg: Package,
      expr: Syntax.Expr): Value[Function] = ???
}

object Main {
  def main(args: Array[String]): Unit = {
    val classWriter = new ClassWriter(0)
    classWriter.visit(
      Opcodes.V1_5,
      Opcodes.ACC_PUBLIC,
      "org/francesco/asmlambda/Foo",
      null,
      "java/lang/Object",
      Array.empty)
    val classBytes = classWriter.toByteArray

    val classLoader: ClassLoader { def defineClass(name: String, bytes: Array[Byte]): Class[_] } =
      new ClassLoader() {
        def defineClass(name: String, bytes: Array[Byte]): Class[_] =
          defineClass(name, bytes, 0, bytes.length)
      }
    val `class` = classLoader.defineClass("org.francesco.asmlambda.Foo", classBytes)
    println("FOOBAR")
  }
}
 */

object Main {
  def main(args: Array[String]): Unit = {
    println("FOOBAR")
  }
}
