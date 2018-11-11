package org.francesco.asmlambda.runtime;

/**
 * This class can be used when {@link Object#equals(Object)}, {@link Object#hashCode()}, or {@link Object#toString()}
 * is needed for a value.
 *
 * Note that a {@link WrappedValue} will only be equals to another {@link WrappedValue}, or in other words a naked value
 * will never be equal to the same naked value wrapped in a {@link WrappedValue}.
 */
public final class WrappedValue {
  public Object value;

  public WrappedValue(Object value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object that) {
    if (!(that instanceof WrappedValue)) {
      return false;
    }
    return (Boolean) Value.eq(this.value, ((WrappedValue) that).value);
  }

  @Override
  public int hashCode() {
    return (Integer) Value.hashCode(this);
  }

  @Override
  public String toString() {
    return (String) Value.toText(this);
  }
}
