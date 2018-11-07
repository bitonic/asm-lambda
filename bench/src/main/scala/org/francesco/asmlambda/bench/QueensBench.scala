package org.francesco.asmlambda.bench

import org.francesco.asmlambda.compiler.{Compiler, LambdaLift, Parser, Rename}
import org.openjdk.jmh.annotations.{Benchmark, Scope, State}

object QueensBench {
  @State(Scope.Benchmark)
  class Program {
    val run: () => Unit = {
      val source =
        """
        def nil() = [];
        def cons(x, xs) = [x, xs];

        def isNil(xs) = length(xs) == 0;

        def head(xs) = xs[0];
        def tail(xs) = xs[1];

        def map(f, xs) = if isNil(xs)
          then nil()
          else cons(f(head(xs)), map(f, tail(xs)));

        def append(xs, ys) = if isNil(xs)
          then ys
          else cons(head(xs), append(tail(xs), ys));

        def concatMap(f, xs) = if isNil(xs)
          then xs
          else append(f(head(xs)), concatMap(f, tail(xs)));

        def zipWith(f, xs, ys) = if isNil(xs) || isNil(ys)
          then nil()
          else
            cons(f(head(xs), head(ys)), zipWith(f, tail(xs), tail(ys)));

        def replicate(n, x) = if n <= 0
          then nil()
          else cons(x, replicate(n - 1, x));

        def listLength(xs) = if isNil(xs)
          then 0
          else 1 + listLength(tail(xs));

        def diff(xs, ys) = if isNil(xs)
          then nil()
          else
            if isNil(ys)
              then xs
              else
                let x = head(xs);
                let y = head(ys);
                if x < y
                  then cons(x, diff(tail(xs), ys))
                  else if x == y
                    then diff(tail(xs), tail(ys))
                    else diff(xs, tail(ys));

        def range(m, n) = if m <= n
          then cons(m, range(m + 1, n))
          else nil();

        def solveAux(ints, kss) = if isNil(kss)
          then cons(nil(), nil())
          else concatMap(
            \(k) ->
              map(
                \(t) -> cons(k, t),
                solveAux(
                  ints,
                  zipWith(
                    \(ls, i) -> diff(ls, cons(k-i, cons(k, cons(k+i, nil())))),
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
  def queens(state: QueensBench.Program): Unit = state.run()
}
