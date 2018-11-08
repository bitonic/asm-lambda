package org.francesco.asmlambda.runtime;

public class Nil {
  private Nil() {}

  public static Nil nil = new Nil();

  @Override
  public boolean equals(Object that) {
    return that instanceof Nil;
  }

  @Override
  public String toString() {
    return "Nil";
  }
}
