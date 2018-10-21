package org.francesco.asmlambda;

public class Functions {
  @FunctionalInterface
  public interface Function0 {
    Object apply();
  }

  @FunctionalInterface
  public interface Function1 {
    Object apply(Object a);
  }

  @FunctionalInterface
  public interface Function2 {
    Object apply(Object a, Object b);
  }

  @FunctionalInterface
  public interface Function3 {
    Object apply(Object a, Object b, Object c);
  }

  @FunctionalInterface
  public interface Function4 {
    Object apply(Object a, Object b, Object c, Object d);
  }

  @FunctionalInterface
  public interface Function5 {
    Object apply(Object a, Object b, Object c, Object d, Object e);
  }

  @FunctionalInterface
  public interface Function6 {
    Object apply(Object a, Object b, Object c, Object d, Object e, Object f);
  }

  @FunctionalInterface
  public interface Function7<A, B, C, D, E, F, G, H> {
    H apply(A a, B b, C c, D d, E e, F f, G g);
  }

  @FunctionalInterface
  public interface Function8<A, B, C, D, E, F, G, H, J> {
    J apply(A a, B b, C c, D d, E e, F f, G g, H h);
  }

  @FunctionalInterface
  public interface Function9<A, B, C, D, E, F, G, H, J, K> {
    K apply(A a, B b, C c, D d, E e, F f, G g, H h, J j);
  }

  @FunctionalInterface
  public interface Function10<A, B, C, D, E, F, G, H, J, K, I> {
    I apply(A a, B b, C c, D d, E e, F f, G g, H h, J j, K k);
  }
}
