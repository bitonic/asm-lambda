package org.francesco.asmlambda.runtime;

public class Cons {
  public Object car;
  public Object cdr;

  public Cons(Object car, Object cdr) {
    this.car = car;
    this.cdr = cdr;
  }

  @Override
  public boolean equals(Object that) {
    try {
      return PrimOp.eqBool(this, that);
    } catch (PrimOpError e) {
      throw new RuntimeException("Malformed Cons -- got PrimOpError " + e + " in equals()");
    }
  }

  @Override
  public String toString() {
    return "Cons(" + car.toString() + ", " + cdr.toString() + ")";
  }
}
