package org.francesco.asmlambda.runtime;

import java.util.*;

/**
 * Values are represented as follows:
 * <p>
 * * Unit => null
 * * I64 => {@link Long}
 * * F64 => {@link Double}
 * * Text => {@link String}
 * * Bool => {@link Boolean}
 * * Symbol => {@link Symbol}
 * * Vector => {@link Object[]}
 * * Map => {@link HashMap}
 * * Set => {@link HashSet}
 * * functions => {@link Functions.Function}
 *
 * Generally speaking we _do not_ define our own classes for values, as you can see above. {@link Symbol} and
 * {@link Functions.Function} are the exception because the Java ecosystem does not provide us with some ready-made
 * solution for those types.
 *
 * The main reason is that we do want to waste space boxing arrays and hash maps.
 *
 * Conceptually though, the types above define the universe of asm-lambda values, and this {@link Value} class defines
 * the methods on values as static functions.
 */
public final class Value {
  private enum ToTextInstruction {
    VALUE,
    COMMA,
    SPACE,
    CLOSE_BRACE,
    CLOSE_BRACKET,
    CLOSE_PARENS,
  }

  /**
   * Converts a value to a parseable string. In other words, you should be able to paste the output of this in
   * asm-lambda source code.
   */
  public static Object toText(Object e0) {
    var instructions = new Stack<ToTextInstruction>();
    instructions.push(ToTextInstruction.VALUE);
    var values = new Stack<>();
    values.push(e0);

    var stringBuilder = new StringBuilder();

    while (!instructions.empty()) {
      var instruction = instructions.pop();

      switch (instruction) {
        case VALUE:
          var value = values.pop();

          if (value == null) {
            stringBuilder.append("nil");
          } else if (value instanceof Long) {
            stringBuilder.append(value);
          } else if (value instanceof Double) {
            stringBuilder.append(value);
          } else if (value instanceof String) {
            escapeString(stringBuilder, (String) value);
          } else if (value instanceof Boolean) {
            stringBuilder.append(value);
          } else if (value instanceof Symbol) {
            stringBuilder.append(":");
            stringBuilder.append(((Symbol) value).string);
          } else if (value instanceof Object[]) {
            Object[] vec = (Object[]) value;
            stringBuilder.append("[");
            instructions.push(ToTextInstruction.CLOSE_BRACKET);
            boolean first = true;
            for (int i = vec.length - 1; i >= 0; i--) {
              if (first) {
                first = false;
              } else {
                instructions.push(ToTextInstruction.SPACE);
              }
              instructions.push(ToTextInstruction.VALUE);
              values.push(vec[i]);
            }
          } else if (value instanceof HashMap) {
            HashMap rec = (HashMap) value;
            stringBuilder.append("{");
            instructions.push(ToTextInstruction.CLOSE_BRACE);
            Object[] keys = rec.keySet().toArray(new Object[0]);
            boolean first = true;
            for (int i = keys.length - 1; i >= 0; i--) {
              var k = keys[i];
              var v = rec.get(k);
              if (first) {
                first = false;
              } else {
                instructions.push(ToTextInstruction.COMMA);
              }
              instructions.push(ToTextInstruction.VALUE);
              values.push(v);
              instructions.push(ToTextInstruction.SPACE);
              instructions.push(ToTextInstruction.VALUE);
              values.push(k);
            }
          } else if (value instanceof HashSet) {
            HashSet set = (HashSet) value;
            stringBuilder.append("#{");
            instructions.push(ToTextInstruction.CLOSE_BRACE);
            boolean first = true;
            for (Object v : set) {
              if (first) {
                first = false;
              } else {
                instructions.push(ToTextInstruction.SPACE);
              }
              instructions.push(ToTextInstruction.VALUE);
              values.push(v);
            }
          } else if (value instanceof Functions.Function) {
            stringBuilder.append("<function>");
          } else if (value instanceof WrappedValue) {
            // optimization so that we can use the HashMap / HashSet method directly rather than mapGet / mapKeys /
            // setContains / setKeys
            instructions.push(ToTextInstruction.VALUE);
            values.push(((WrappedValue) value).value);
          } else {
            throw new RuntimeException("Expected value in toText(), but got " + value.getClass());
          }
          break;
        case CLOSE_BRACE:
          stringBuilder.append("}");
          break;
        case CLOSE_PARENS:
          stringBuilder.append(")");
          break;
        case CLOSE_BRACKET:
          stringBuilder.append("]");
          break;
        case COMMA:
          stringBuilder.append(", ");
          break;
        case SPACE:
          stringBuilder.append(" ");
          break;
      }
    }

    return stringBuilder.toString();
  }

  private static void escapeCodepoint(StringBuilder stringBuilder, int codePoint) {
    stringBuilder.append("\\u");
    stringBuilder.append(String.format("%04X", codePoint));
  }

  // keep in sync with string parsing in Sexp.scala
  public static void escapeString(StringBuilder stringBuilder, String s) {
    stringBuilder.append("\"");

    for (int i = 0; i < s.length(); ) {
      int codePoint = s.codePointAt(i);
      char[] chars = Character.toChars(codePoint);

      if (chars.length == 1) {
        char ch = chars[0];

        switch (ch) {
          case '\\':
            stringBuilder.append("\\\\");
            break;
          case '\"':
            stringBuilder.append("\\\"");
            break;
          case '\n':
            stringBuilder.append("\\n");
            break;
          case '\t':
            stringBuilder.append("\\t");
            break;
          default:
            if (ch == ' ' || (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') || (ch >= '0' && ch <= '9')) {
              stringBuilder.append(ch);
            } else {
              escapeCodepoint(stringBuilder, codePoint);
            }
            break;
        }
      } else {
        escapeCodepoint(stringBuilder, codePoint);
      }

      i += chars.length;
    }

    stringBuilder.append("\"");
  }

  private static boolean eqBool(Object e1, Object e2) {
    var left = new Stack<>();
    left.push(e1);
    var right = new Stack<>();
    right.push(e2);

    while (!left.empty()) {
      var l = left.pop();
      var r = right.pop();

      if (l == null && r == null) {
        // no-op
      } else if (l instanceof Long && r instanceof Long) {
        if (!l.equals(r)) return false;
      } else if (l instanceof Double && r instanceof Double) {
        if (!l.equals(r)) return false;
      } else if (l instanceof String && r instanceof String) {
        if (!l.equals(r)) return false;
      } else if (l instanceof Boolean && r instanceof Boolean) {
        if (!l.equals(r)) return false;
      } else if (l instanceof Symbol && r instanceof Symbol) {
        var sym1 = (Symbol) l;
        var sym2 = (Symbol) r;
        if (sym1.token != sym2.token) return false;
      } else if (l instanceof Object[] && r instanceof Object[][]) {
        var arr1 = (Object[]) l;
        var arr2 = (Object[]) r;
        if (arr1.length != arr2.length) {
          return false;
        }
        for (var i = 0; i < arr1.length; i++) {
          left.push(arr1[i]);
          right.push(arr2[i]);
        }
      } else if (l instanceof WrappedValue && r instanceof WrappedValue) {
        // optimization so that we can use the HashMap / HashSet method directly rather than mapGet / mapKeys /
        // setContains / setKeys
        left.push((WrappedValue) l);
        right.push((WrappedValue) r);
      } else if (l instanceof HashSet && r instanceof HashSet) {
        var set1 = (HashSet) l;
        var set2 = (HashSet) r;
        if (set1.size() != set2.size()) {
          return false;
        }
        for (Object el : set1) {
          if (!set2.contains(set1)) {
            return false;
          }
        }
      } else if (l instanceof HashMap && r instanceof HashMap) {
        var rec1 = (HashMap) l;
        var rec2 = (HashMap) r;
        if (rec1.size() != rec2.size()) {
          return false;
        }
        for (Object k : rec1.keySet()) {
          var v1 = rec1.get(k);
          var v2 = rec2.get(k);
          if (v2 != null) {
            left.push(v1);
            right.push(v2);
          } else {
            return false;
          }
        }
      } else {
        // note that we refuse to compare functions
        return false;
      }
    }

    return true;
  }

  public static Object eq(Object e1, Object e2) {
      return eqBool(e1, e2);
  }

  public static Object hashCode(Object e0) {
    var toHash = new Stack<>();
    toHash.push(e0);

    int hash = 1;

    /*
     * we don't really hash in a "tagged" way, for example an array might have the same hash as a map. but that's ok, we
     * optimize for the common case -- when the key type is homogeneous.
     */
    while(!toHash.isEmpty()) {
      var v = toHash.pop();

      if (v == null) {
        // no op
      } else if (v instanceof Long || v instanceof Double || v instanceof String || v instanceof Boolean || v instanceof Symbol) {
        // scalars go in directly
        hash = 31 * hash + v.hashCode();
      } else if (v instanceof Object[]) {
        var arr = (Object[]) v;
        for (Object el : arr) {
          toHash.push(el);
        }
      } else if (v instanceof HashMap) {
        HashMap<Object, Object> map = (HashMap<Object, Object>) v;
        for (Map.Entry<Object, Object> entry : map.entrySet()) {
          toHash.push(entry.getValue());
          toHash.push(entry.getKey());
        }
      } else if (v instanceof HashSet) {
        HashSet set = (HashSet) v;
        for (Object el : set) {
          toHash.push(el);
        }
      } else if (v instanceof WrappedValue) {
        // optimization so that we can use the HashMap / HashSet method directly rather than mapGet / mapKeys /
        // setContains / setKeys
        toHash.push(((WrappedValue) v).value);
      } else {
        throw new RuntimeException("Couldn't hash bad value of type " + v.getClass());
      }
    }

    return hash;
  }

  /**
   * We need to wrap compound keys when we put them in sets / maps to have proper {@link Object#hashCode()} and
   * {@link Object#equals(Object)}. The primitive operations defined here do the wrapping and unwrapping as appropriate.
   */
  private static Object wrapNonScalars(Object value) {
    if (value instanceof Object[] || value instanceof HashMap || value instanceof HashSet) {
      return new WrappedValue(value);
    }
    return value;
  }

  private static Object unwrap(Object value) {
    if (value instanceof WrappedValue) {
      return ((WrappedValue) value).value;
    }
    return value;
  }

  public static Object mapNew() {
    return new HashMap();
  }

  public static Object mapKeys(Object map0) {
    if (!(map0 instanceof HashMap)) {
      throw new RuntimeException("Can't call mapKeys on value of type " + map0.getClass());
    }
    HashMap map = (HashMap) map0;

    Object[] keys = new Object[map.size()];
    int ix = 0;
    for (Object k : map.keySet()) {
      keys[ix] = unwrap(k);
      ix++;
    }

    return keys;
  }

  public static Object mapPut(Object map0, Object k, Object v) {
    if (!(map0 instanceof HashMap)) {
      throw new RuntimeException("Can't call mapPut on value of type " + map0.getClass());
    }
    HashMap map = (HashMap) map0;

    map.put(wrapNonScalars(k), v);

    return null;
  }

  public static Object mapContains(Object map0, Object k) {
    if (!(map0 instanceof HashMap)) {
      throw new RuntimeException("Can't call mapGet on value of type " + map0.getClass());
    }
    HashMap map = (HashMap) map0;

    return map.containsKey(wrapNonScalars(k));
  }

  public static Object mapGet(Object map0, Object k) {
    if (!(map0 instanceof HashMap)) {
      throw new RuntimeException("Can't call mapGet on value of type " + map0.getClass());
    }
    HashMap map = (HashMap) map0;

    var wrappedKey = wrapNonScalars(k);

    // we can't simply check for null after get we have null as a possible value.
    if (!map.containsKey(wrappedKey)) {
      throw new RuntimeException("Can't fetch key " + wrappedKey.toString() + " in mapGet");
    }

    return map.get(wrappedKey);
  }

  public static Object setNew() {
    return new HashSet();
  }

  public static Object setKeys(Object set0) {
    if (!(set0 instanceof HashSet)) {
      throw new RuntimeException("Can't call setKeys on value of type " + set0.getClass());
    }
    HashSet set = (HashSet) set0;

    Object[] keys = new Object[set.size()];
    int ix = 0;
    for (Object k : set) {
      keys[ix] = unwrap(k);
      ix++;
    }

    return keys;
  }

  public static Object setAdd(Object set0, Object k) {
    if (!(set0 instanceof HashSet)) {
      throw new RuntimeException("Can't call setAdd on value of type " + set0.getClass());
    }
    HashSet set = (HashSet) set0;

    set.add(wrapNonScalars(k));

    return null;
  }

  public static Object setContains(Object set0, Object k) {
    if (!(set0 instanceof HashSet)) {
      throw new RuntimeException("Can't call setAdd on value of type " + set0.getClass());
    }
    HashSet set = (HashSet) set0;

    return set.contains(wrapNonScalars(k));
  }

  public static Object add(Object e1, Object e2) {
    if (e1 instanceof Long && e2 instanceof Long) {
      return (Long) e1 + (Long) e2;
    }
    if (e1 instanceof Double && e2 instanceof Double) {
      return (Double) e1 + (Double) e2;
    }
    if (e1 instanceof String && e2 instanceof String) {
      return (String) e1 + (String) e2;
    }
    if (e1 instanceof Object[] && e2 instanceof Object[]) {
      var arr1 = (Object[]) e1;
      var arr2 = (Object[]) e2;
      Object[] result = new Object[arr1.length + arr2.length];
      System.arraycopy(arr1, 0, result, 0, arr1.length);
      System.arraycopy(arr2, 0, result, arr1.length, arr2.length);
      return result;
    }
    throw new RuntimeException(
        "Cannot add operands of type " + e1.getClass() + " and type " + e2.getClass());
  }

  public static Object sub(Object e1, Object e2) {
    if (e1 instanceof Long && e2 instanceof Long) {
      return (Long) e1 - (Long) e2;
    }
    if (e1 instanceof Double && e2 instanceof Double) {
      return (Double) e1 - (Double) e2;
    }
    throw new RuntimeException(
        "Cannot subtract operands of type " + e1.getClass() + " and type " + e2.getClass());
  }

  public static Object mul(Object e1, Object e2) {
    if (e1 instanceof Long && e2 instanceof Long) {
      return (Long) e1 * (Long) e2;
    }
    if (e1 instanceof Double && e2 instanceof Double) {
      return (Double) e1 * (Double) e2;
    }
    throw new RuntimeException(
        "Cannot multipy operands of type " + e1.getClass() + " and type " + e2.getClass());
  }

  public static Object div(Object e1, Object e2) {
    if (e1 instanceof Long && e2 instanceof Long) {
      return (Long) e1 / (Long) e2;
    }
    if (e1 instanceof Double && e2 instanceof Double) {
      return (Double) e1 / (Double) e2;
    }
    throw new RuntimeException(
        "Cannot divide operands of type " + e1.getClass() + " and type " + e2.getClass());
  }

  public static Object less(Object e1, Object e2) {
    if (e1 instanceof Long && e2 instanceof Long) {
      return ((Long) e1) < ((Long) e2);
    }
    if (e1 instanceof Double && e2 instanceof Double) {
      return ((Double) e1) < ((Double) e2);
    }
    throw new RuntimeException(
        "Cannot < operands of type " + e1.getClass() + " and type " + e2.getClass());
  }

  public static Object lessEq(Object e1, Object e2) {
    if (e1 instanceof Long && e2 instanceof Long) {
      return ((Long) e1) <= ((Long) e2);
    }
    if (e1 instanceof Double && e2 instanceof Double) {
      return ((Double) e1) <= ((Double) e2);
    }
    throw new RuntimeException(
        "Cannot <= operands of type " + e1.getClass() + " and type " + e2.getClass());
  }

  public static Object greater(Object e1, Object e2) {
    if (e1 instanceof Long && e2 instanceof Long) {
      return ((Long) e1) > ((Long) e2);
    }
    if (e1 instanceof Double && e2 instanceof Double) {
      return ((Double) e1) > ((Double) e2);
    }
    throw new RuntimeException(
        "Cannot > operands of type " + e1.getClass() + " and type " + e2.getClass());
  }

  public static Object greaterEq(Object e1, Object e2) {
    if (e1 instanceof Long && e2 instanceof Long) {
      return ((Long) e1) >= ((Long) e2);
    }
    if (e1 instanceof Double && e2 instanceof Double) {
      return ((Double) e1) >= ((Double) e2);
    }
    throw new RuntimeException(
        "Cannot >= operands of type " + e1.getClass() + " and type " + e2.getClass());
  }

  public static Object or(Object e1, Object e2) {
    if (e1 instanceof Boolean && e2 instanceof Boolean) {
      return ((Boolean) e1) || ((Boolean) e2);
    }
    throw new RuntimeException(
        "Cannot || operands of type " + e1.getClass() + " and type " + e2.getClass());
  }

  public static Object and(Object e1, Object e2) {
    if (e1 instanceof Boolean && e2 instanceof Boolean) {
      return ((Boolean) e1) && ((Boolean) e2);
    }
    throw new RuntimeException(
        "Cannot && operands of type " + e1.getClass() + " and type " + e2.getClass());
  }

  public static Object vectorGet(Object e0, Object ix0) {
    if (!(e0 instanceof Object[] && ix0 instanceof Long)) {
      throw new RuntimeException("Cannot run vectorGet on arguments of type " + e0.getClass() + " and " + ix0.getClass());
    }
    var arr = (Object[]) e0;
    var ix = (int) (long) ix0;
    return arr[ix];
  }

  public static Object vectorSet(Object arr0, Object ix0, Object v) {
    if (!(arr0 instanceof Object[] && ix0 instanceof Long)) {
      throw new RuntimeException("Cannot run vectorSet on arguments of type " + arr0.getClass() + " and " + ix0.getClass());
    }
    var arr = (Object[]) arr0;
    var ix = (int) (long) ix0;
    arr[ix] = v;
    return null;
  }
}
