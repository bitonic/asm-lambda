package org.francesco.asmlambda;

import java.util.HashMap;

public class Record {
  private HashMap<String, Object> elements;

  /** The elements must all be valid values. */
  public Record(HashMap<String, Object> elements) {
    this.elements = elements;
  }

  /** The element must all be a valid value. */
  public Record update(String key, Object value) {
    var newElements = new HashMap<String, Object>(elements);
    newElements.put(key, value);
    return new Record(newElements);
  }

  public HashMap<String, Object> getElements() {
    return elements;
  }

  @Override
  public boolean equals(Object that) {
    try {
      return PrimOp.eqBool(this, that);
    } catch (PrimOpError e) {
      throw new RuntimeException("Malformed Records -- got PrimOpError " + e + " in equals()");
    }
  }

  @Override
  public String toString() {
    try {
      return (String) PrimOp.toText(this);
    } catch (PrimOpError e) {
      throw new RuntimeException("Malformed Records -- got PrimOpError " + e + " in toString()");
    }
  }
}

