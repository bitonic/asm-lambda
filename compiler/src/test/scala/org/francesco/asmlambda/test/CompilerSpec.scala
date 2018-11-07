package org.francesco.asmlambda.test

import org.francesco.asmlambda.compiler.{Compiler, LambdaLift, Parser, Rename}
import org.francesco.asmlambda.compiler.{Syntax => S}
import java.lang.reflect.InvocationTargetException

import org.francesco.asmlambda.compiler.{LambdaLift, Parser, Rename, Syntax}
import org.scalatest.{FreeSpec, Matchers}

import scala.collection.mutable.ArraySeq
import scala.language.implicitConversions

class CompilerSpec extends FreeSpec with Matchers {
  "Rename" - {
    import Syntax.Expr
    import Syntax.{Expr => E}
    import Syntax.{Prim => P}

    implicit def exprStr(s: String): Expr = fastparse.parse(s, Parser.exprOnly(_)).get.value

    "rename in lambda" in {
      Rename.expr(Map.empty, """\(x, x, y, y) -> x(y)""") shouldBe
        E.Lam(ArraySeq("x", "x$1", "y", "y$1"), E.mkApp(E.Var("x$1"), E.Var("y$1")))
    }

    "rename in lambda (app)" in {
      Rename.expr(Map.empty, """(\(x, x) -> x)(\(x, x) -> x)""") shouldBe
        E.mkApp(
          E.Lam(ArraySeq("x", "x$1"), E.Var("x$1")),
          E.Lam(ArraySeq("x", "x$1"), E.Var("x$1")))
    }

    "rename in let" in {
      val e: Expr =
        """
          let x = 42;
          let y = x;
          let x = 13;
          let y = y(x);
          x(y)
        """
      val expected =
        E.Let(
          "x",
          E.Prim(P.I64(42)),
          E.Let(
            "y",
            E.Var("x"),
            E.Let(
              "x$1",
              E.Prim(P.I64(13)),
              E.Let("y$1", E.mkApp(E.Var("y"), E.Var("x$1")), E.mkApp(E.Var("x$1"), E.Var("y$1")))))
        )
      Rename.expr(Map.empty, e) shouldBe expected
    }

    "rename defs" in {
      Rename.`package`(
        S.Package(Map("foo" -> S.Definition(ArraySeq.empty, """let foo = 42; foo""")), E.Prim(P.I64(42)))) shouldBe
        S.Package(
          Map("foo" -> S
            .Definition(ArraySeq.empty, E.Let("foo$1", E.Prim(P.I64(42)), E.Var("foo$1")))),
          E.Prim(P.I64(42)))
    }
  }

  "LambdaLift" - {
    import org.francesco.asmlambda.compiler.Syntax.{Prim => SP}
    import org.francesco.asmlambda.compiler.Syntax.{PrimOp => SPOP}
    import org.francesco.asmlambda.compiler.Compiler._
    import org.francesco.asmlambda.compiler.Compiler.{Expr => E}

    implicit def pkgStr(s: String): Syntax.Package = fastparse.parse(s, Parser.`package`(_)).get.value

    "super simple" in {
      LambdaLift.`package`("""
        def foo() = 42;
        7
      """) shouldBe
        Package(Map("foo" -> Definition(ArraySeq.empty, ArraySeq.empty, E.Prim(SP.I64(42)))), E.Prim(SP.I64(7)))
    }

    "lambda (in def)" in {
      LambdaLift.`package`("""
          def foo(x) = \(y) -> x + y;
          42
        """) shouldBe
        Package(
          Map(
            "foo$0" -> Definition(
              ArraySeq("x"),
              ArraySeq("y"),
              E.PrimOpCall(SPOP.Add, ArraySeq(E.Var("x"), E.Var("y")))),
            "foo" -> Definition(ArraySeq.empty, ArraySeq("x"), E.Def("foo$0", ArraySeq("x"), 1))
          ),
          E.Prim(SP.I64(42)))
    }

    "lambda (in let)" in {
      val pkg =
        """
            let id = \(x) -> x;
            id(42)
          """
      LambdaLift.`package`(pkg) shouldBe
          Package(
            Map("main$0" -> Definition(ArraySeq.empty, ArraySeq("x"), E.Var("x"))),
            E.Let("id", E.Def("main$0",ArraySeq(), 1), E.DynamicCall(E.Var("id"), ArraySeq(E.Prim(S.Prim.I64(42))))))
    }

    "def (applied)" in {
      LambdaLift.`package`("""
          def foo(x) =
            def bar(y) = x + y;
            bar(42);
          42
        """) shouldBe
        Package(
          Map(
            "foo:bar" -> Definition(
              ArraySeq("x"),
              ArraySeq("y"),
              E.PrimOpCall(SPOP.Add, ArraySeq(E.Var("x"), E.Var("y")))),
            "foo" -> Definition(
              ArraySeq(),
              ArraySeq("x"),
              E.StaticCall("foo:bar", ArraySeq("x"), ArraySeq(E.Prim(SP.I64(42)))))
          ),
          E.Prim(SP.I64(42)))
    }

    "def (not applied)" in {
      LambdaLift.`package`("""
          def foo(x) =
            def bar(y) = x + y;
            bar;
          42
        """) shouldBe
          Package(
            Map(
              "foo:bar" -> Definition(
                ArraySeq("x"),
                ArraySeq("y"),
                E.PrimOpCall(SPOP.Add, ArraySeq(E.Var("x"), E.Var("y")))),
              "foo" -> Definition(
                ArraySeq.empty,
                ArraySeq("x"),
                E.Def("foo:bar", ArraySeq("x"), 1))
            ),
            E.Prim(SP.I64(42)))
    }
  }

  "Compile" - {
    import org.francesco.asmlambda.compiler.LambdaLift
    import org.francesco.asmlambda.compiler.Value._
    import org.francesco.asmlambda.compiler.Compiler.Package

    implicit def pkgStr(s: String): Package =
       LambdaLift.`package`(Rename.`package`(fastparse.parse(s, Parser.`package`(_)).get.value))

    "methodDescriptor" - {
      "0 arity" in {
        Compiler.methodDescriptor(0).toMethodDescriptorString shouldBe "()Ljava/lang/Object;"
      }

      "1 arity" in {
        Compiler.methodDescriptor(1).toMethodDescriptorString shouldBe "(Ljava/lang/Object;)Ljava/lang/Object;"
      }

      "2 arity" in {
        Compiler.methodDescriptor(2).toMethodDescriptorString shouldBe "(Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;"
      }
    }

    def run(pkg: Package): Object = {
      /*
      import org.objectweb.asm.ClassReader
      import org.objectweb.asm.util.TraceClassVisitor
      import java.io.PrintWriter
      */

      /*
      println(s"Package: \n$pkg")
      */

      val className = "org.francesco.asmlambda.test.CompiledPackage"

      /*
      val bytecode: Array[Byte] = Compiler(className, pkg)
      val classReader: ClassReader = new ClassReader(bytecode)
      classReader.accept(new TraceClassVisitor(new PrintWriter(System.out)), 0)
      */

      try {
        Compiler.run(className, pkg)
      } catch {
        case ite: InvocationTargetException =>
          ite.getCause match {
            case bme: BootstrapMethodError =>
              println(s"Got bootstrap method error with cause ${bme.getCause}")
            case _ =>
              println(s"Got InvocationTargetException with cause ${ite.getCause}")
          }
          throw ite
      }
    }

    "prim" - {
      "I64 literal" in {
        run( """42""") shouldBe 42.getValue
      }

      "I64 add" in {
        run("""3 + 4""") shouldBe (3 + 4).getValue
      }

      "I64 arithmetic" in {
        run("""1 * 2 + 12 / 4 - 5 * 6""") shouldBe (1 * 2 + 12 / 4 - 5 * 6).getValue
      }

      "F64 literal" in {
        run("""42.24""") shouldBe 42.24.getValue
      }

      "F64 add" in {
        run("""3.24 + 4.56""") shouldBe (3.24 + 4.56).getValue
      }

      "F64 arithmetic" in {
        run("""1.35 * 2.86 + 12.34 / 4.468 - 5.666 * 6.903""") shouldBe (1.35 * 2.86 + 12.34 / 4.468 - 5.666 * 6.903).getValue
      }

      "I64 equality, false" in {
        run("""3 == 2""") shouldBe false.getValue
      }

      "I64 equality, true" in {
        run("""3 == 3""") shouldBe true.getValue
      }

      "Text literal" in {
        run(""""foo"""") shouldBe "foo".getValue
      }

      "Text concat" in {
        run(""""foo" + "bar"""") shouldBe "foobar".getValue
      }
    }

    "def (StaticCall)" - {
      "I64 literal, no arguments" in {
        run("""def foo() = 42; foo()""") shouldBe 42.getValue
      }

      "identity, I64 literal argument" in {
        run("""def foo(x) = x; foo(42)""") shouldBe 42.getValue
      }

      "I64 arith, non-commutative" in {
        run("""def foo(x, y, z) = x - y - z; foo(10, 5, 7)""") shouldBe (-2).getValue
      }

      "Text concat" in {
        run("""def foo(x, y) = x + y; foo("x", "y")""") shouldBe "xy".getValue
      }

      "captured arg, I64" in {
        val pkg =
          """
            def foo(x) =
              def bar(y) = x + y;
              bar(4);
            foo(3)
          """
        run(pkg) shouldBe 7.getValue
      }

      "reference" in {
        val pkg =
          """
            def foo(x) = x * x;
            let f = foo;
            f(7)
          """
        run(pkg) shouldBe (7 * 7).getValue
      }
    }

    "let" - {
      "one I64" in {
        run("""let x = 42; x""") shouldBe 42.getValue
      }

      "two I64" in {
        run ("""let x = 3; let y = 4; x + y""") shouldBe 7.getValue
      }
    }

    "ITE" - {
      "simple (true)" in {
        run("""if true then 1 else 2""") shouldBe 1.getValue
      }

      "simple (false)" in {
        run("""if false then 1 else 2""") shouldBe 2.getValue
      }
    }

    "lambda" - {
      "identity" in {
        val pkg =
          """
            let id = \(x) -> x;
            id(42)
          """
        run(pkg) shouldBe 42.getValue
      }

      "const" in {
        val pkg =
          """
            let const = \(x, y) -> x;
            const(1, 2)
          """
        run(pkg) shouldBe 1.getValue
      }

      "capture" in {
        val pkg =
          """
            let x = 42;
            let f = \(y) -> x + y;
            f(3)
          """
        run(pkg) shouldBe 45.getValue
      }
    }

    "record" - {
      "empty" in {
        run("""{}""") shouldBe Record().getValue
      }

      "one" in {
        run("""{foo = true}""") shouldBe Record("foo" -> true).getValue
      }

      "two" in {
        run("""{foo = true, bar = false}""") shouldBe Record("foo" -> true, "bar" -> false).getValue
      }

      "nested" in {
        run("""{foo = {bar = 1}}""") shouldBe Record("foo" -> Record("bar" -> 1)).getValue
      }

      "update (rewrite)" in {
        run("""{foo = 1}{foo = 2}""") shouldBe Record("foo" -> 2).getValue
      }

      "update (new)" in {
        run("""{foo = 1}{bar = 2}""") shouldBe Record("foo" -> 1, "bar" -> 2).getValue
      }

      "lookup" in {
        run("""{foo = 1}.foo""") shouldBe 1.getValue
      }
    }
  }
}
