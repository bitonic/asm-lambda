package org.francesco.asmlambda.bench

import java.util.concurrent.TimeUnit

import org.francesco.asmlambda.compiler.{Compiler, LambdaLift, Parser, Rename}
import org.openjdk.jmh.annotations._

object QueensBench {
  @State(Scope.Benchmark)
  class Program {
    val run: () => Unit = {
      val source =
        """
        // we use [] for nil, [x, xs] for cons

        def isNil(xs) = length(xs) == 0;

        def head(xs) = xs[0];
        def tail(xs) = xs[1];

        def map(f, xs) = if isNil(xs)
          then []
          else [f(head(xs)), map(f, tail(xs))];

        def append(xs, ys) = if isNil(xs)
          then ys
          else [head(xs), append(tail(xs), ys)];

        def concatMap(f, xs) = if isNil(xs)
          then xs
          else append(f(head(xs)), concatMap(f, tail(xs)));

        def zipWith(f, xs, ys) = if isNil(xs) || isNil(ys)
          then []
          else
            [f(head(xs), head(ys)), zipWith(f, tail(xs), tail(ys))];

        def replicate(n, x) = if n <= 0
          then []
          else [x, replicate(n - 1, x)];

        def listLength(xs) = if isNil(xs)
          then 0
          else 1 + listLength(tail(xs));

        def diff(xs, ys) = if isNil(xs)
          then []
          else
            if isNil(ys)
              then xs
              else
                let x = head(xs);
                let y = head(ys);
                if x < y
                  then [x, diff(tail(xs), ys)]
                  else if x == y
                    then diff(tail(xs), tail(ys))
                    else diff(xs, tail(ys));

        def range(m, n) = if m <= n
          then [m, range(m + 1, n)]
          else [];

        def solveAux(ints, kss) = if isNil(kss)
          then [[], []]
          else concatMap(
            \(k) ->
              map(
                \(t) -> [k, t],
                solveAux(
                  ints,
                  zipWith(
                    \(ls, i) -> diff(ls, [k-i, [k, [k+i, []]]]),
                    tail(kss),
                    ints))),
            head(kss));

        def solve(n) =
          let ints = range(1, n);
          solveAux(ints, replicate(n, ints));

        listLength(solve(10))
      """
      val pkg = LambdaLift.`package`(
        Rename.`package`(fastparse.parse(source, Parser.`package`(_)).get.value))
      val run = Compiler.compileToFunction("org.francesco.asmlambda.bench.Queens", pkg)
      () => {
        val res = run.apply()
        assert(res == java.lang.Long.valueOf(724))
      }
    }
  }
}
class QueensBench {
  @Benchmark
  @OutputTimeUnit(TimeUnit.MILLISECONDS)
  @BenchmarkMode(Array(Mode.AverageTime))
  def asmLambdaQueens(state: QueensBench.Program): Unit = state.run()

  @Benchmark
  @OutputTimeUnit(TimeUnit.MILLISECONDS)
  @BenchmarkMode(Array(Mode.AverageTime))
  def scalaQueens(): Unit = {
    def diff(xs0: List[Int], ys0: List[Int]): List[Int] = xs0 match {
      case Nil => Nil
      case x :: xs => ys0 match {
        case Nil => xs0
        case y :: ys =>
          if (x < y) {
            x :: diff(xs, ys0)
          } else if (x == y) {
            diff(xs, ys)
          } else {
            diff(xs0, ys)
          }
      }
    }

    def range(m: Int, n: Int): List[Int] = if (m <= n) {
      m :: range(m+1, n)
    } else {
      Nil
    }

    def solveAux(ints: List[Int], kss0: List[List[Int]]): List[List[Int]] = kss0 match {
      case Nil => Nil :: Nil
      case ks :: kss =>
        ks.flatMap(k =>
          solveAux(
            ints,
            (kss, ints).zipped map { case (ls, i) => diff(ls, k-i :: k :: k+i :: Nil) }).map(k :: _))
    }

    def solve(n: Int): List[List[Int]] = {
      val ints = range(1, n)
      solveAux(ints, List.fill(n)(ints))
    }

    val res = solve(10).length
    assert(res == 724)
  }
}
