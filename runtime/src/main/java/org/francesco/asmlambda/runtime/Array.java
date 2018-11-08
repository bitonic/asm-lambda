package org.francesco.asmlambda.runtime;

public class Array {
  private Object[] elements;

  public Array(Object[] elements) { this.elements = elements; }

  public Array concat(Array that) {
    var arr1 = elements;
    var arr2 = that.elements;
    Object[] result = new Object[arr1.length + arr2.length];
    System.arraycopy(arr1, 0, result, 0, arr1.length);
    System.arraycopy(arr2, 0, result, arr1.length, arr2.length);
    return new Array(result);
  }

  public Object[] getElements() {
    return elements;
  }

  @Override
  public boolean equals(Object that) {
    try {
      return PrimOp.eqBool(this, that);
    } catch (PrimOpError e) {
      throw new RuntimeException("Malformed Array -- got PrimOpError " + e + " in equals()");
    }
  }

  @Override
  public String toString() {
    try {
      return (String) PrimOp.toText(this);
    } catch (PrimOpError e) {
      throw new RuntimeException("Malformed Array -- got PrimOpError " + e + " in toString()");
    }
  }

  // empty array singleton
  private static Array _empty = null;
  public static Array empty() {
    if (_empty == null) {
      _empty = new Array(new Object[]{});
    }
    return _empty;
  }
}
