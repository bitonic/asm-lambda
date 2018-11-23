package org.francesco.asmlambda.runtime;

public final class Functions {
  /**
   * The universe of asm-lambda function objects
   */
  interface Function {}

  @FunctionalInterface
  public interface Function0 extends Function {
    Object apply();
  }

  @FunctionalInterface
  public interface Function1 extends Function {
    Object apply(Object a);
  }

  @FunctionalInterface
  public interface Function2 extends Function {
    Object apply(Object a, Object b);
  }

  @FunctionalInterface
  public interface Function3 extends Function {
    Object apply(Object a, Object b, Object c);
  }

  @FunctionalInterface
  public interface Function4 extends Function {
    Object apply(Object a, Object b, Object c, Object d);
  }

  @FunctionalInterface
  public interface Function5 extends Function {
    Object apply(Object a, Object b, Object c, Object d, Object e);
  }

  @FunctionalInterface
  public interface Function6 extends Function {
    Object apply(Object a, Object b, Object c, Object d, Object e, Object f);
  }

  @FunctionalInterface
  public interface Function7 extends Function {
    Object apply(Object a, Object b, Object c, Object d, Object e, Object f, Object g);
  }

  @FunctionalInterface
  public interface Function8 extends Function {
    Object apply(Object a, Object b, Object c, Object d, Object e, Object f, Object g, Object h);
  }

  @FunctionalInterface
  public interface Function9 extends Function {
    Object apply(Object a, Object b, Object c, Object d, Object e, Object f, Object g, Object h, Object j);
  }

  @FunctionalInterface
  public interface Function10 extends Function {
    Object apply(Object a, Object b, Object c, Object d, Object e, Object f, Object g, Object h, Object j, Object k);
  }
}
