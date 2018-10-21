package org.francesco.asmlambda;

import org.apache.commons.text.StringEscapeUtils;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.Stack;

public final class PrimOp {
  public static Object add(Object e1, Object e2) throws PrimOpError {
    if (e1 instanceof Long && e2 instanceof Long) {
      return (Long) e1 + (Long) e2;
    }
    if (e1 instanceof Double && e2 instanceof Double) {
      return (Double) e1 + (Double) e2;
    }
    if (e1 instanceof String && e2 instanceof String) {
      return (String) e1 + (String) e2;
    }
    throw new PrimOpError(
        "Cannot add operands of type " + e1.getClass() + " and type " + e2.getClass());
  }

  public static Object sub(Object e1, Object e2) throws PrimOpError {
    if (e1 instanceof Long && e2 instanceof Long) {
      return (Long) e1 - (Long) e2;
    }
    if (e1 instanceof Double && e2 instanceof Double) {
      return (Double) e1 - (Double) e2;
    }
    throw new PrimOpError(
        "Cannot subtract operands of type " + e1.getClass() + " and type " + e2.getClass());
  }

  public static Object mul(Object e1, Object e2) throws PrimOpError {
    if (e1 instanceof Long && e2 instanceof Long) {
      return (Long) e1 * (Long) e2;
    }
    if (e1 instanceof Double && e2 instanceof Double) {
      return (Double) e1 * (Double) e2;
    }
    throw new PrimOpError(
        "Cannot multipy operands of type " + e1.getClass() + " and type " + e2.getClass());
  }

  public static Object div(Object e1, Object e2) throws PrimOpError {
    if (e1 instanceof Long && e2 instanceof Long) {
      return (Long) e1 / (Long) e2;
    }
    if (e1 instanceof Double && e2 instanceof Double) {
      return (Double) e1 / (Double) e2;
    }
    throw new PrimOpError(
        "Cannot divide operands of type " + e1.getClass() + " and type " + e2.getClass());
  }

  public static boolean eqBool(Object e1, Object e2) throws PrimOpError {
    final class Pair {
      final Object fst;
      final Object snd;

      Pair(Object fst, Object snd) {
        this.fst = fst;
        this.snd = snd;
      }
    }

    var toCompare = new Stack<Pair>();
    toCompare.push(new Pair(e1, e2));

    while (!toCompare.empty()) {
      var pair = toCompare.pop();
      var l = pair.fst;
      var r = pair.snd;

      if (l instanceof Long && r instanceof Long) {
        return l.equals(r);
      } else if (l instanceof Double && r instanceof Double) {
        return l.equals(r);
      } else if (l instanceof String && r instanceof String) {
        return l.equals(r);
      } else if (l instanceof Boolean && r instanceof Boolean) {
        return l.equals(r);
      } else if (l instanceof Record && r instanceof Record) {
        var rec1 = ((Record) l).getElements();
        var rec2 = ((Record) r).getElements();
        if (rec1.size() != rec2.size()) {
          return false;
        }
        for (String k : rec1.keySet()) {
          var v1 = rec1.get(k);
          if (rec2.containsKey(k)) {
            var v2 = rec2.get(k);
            toCompare.push(new Pair(v1, v2));
          } else {
            return false;
          }
        }
      } else {
        throw new PrimOpError(
            "Cannot compare operands of type " + e1.getClass() + " and type " + e2.getClass());
      }
    }

    return true;
  }

  public static Object eq(Object e1, Object e2) throws PrimOpError {
    return eqBool(e1, e2);
  }

  public static int boolToInt(Object e) throws PrimOpError {
    if (e instanceof Boolean) {
      return ((Boolean) e) ? 1 : 0;
    }
    throw new PrimOpError("Expected boolean, got " + e.getClass());
  }

  public static Object lookup(Object rec0, String k) throws PrimOpError {
    if (rec0 instanceof Record) {
      Record rec = (Record) rec0;
      Object v = rec.getElements().get(k);
      if (v != null) {
        return v;
      } else {
        throw new PrimOpError("Could not find key " + k + " in record " + rec.toString());
      }
    }
    throw new PrimOpError("Expected Record for lookup, got " + rec0.getClass());
  }

  public static Object update(Object rec0, String k, Object v) throws PrimOpError {
    if (rec0 instanceof Record) {
      Record rec = (Record) rec0;
      return rec.update(k, v);
    }
    throw new PrimOpError("Expected Record for lookup, got " + rec0.getClass());
  }

  private enum ToTextInstruction {VALUE, RECORD_VALUE, RECORD_LAST_VALUE, RECORD_END }

  public static Object toText(Object e0) throws PrimOpError {
    var instructions = new Stack<ToTextInstruction>();
    instructions.push(ToTextInstruction.VALUE);
    var values = new Stack<>();
    values.push(e0);
    var recordLabels = new Stack<String>();

    var stringBuilder = new StringBuilder();

    while (!instructions.empty()) {
      var instruction = instructions.pop();

      if (instruction == ToTextInstruction.VALUE || instruction == ToTextInstruction.RECORD_VALUE || instruction == ToTextInstruction.RECORD_LAST_VALUE) {
        // if this is a record value, first emit the label
        if (instruction == ToTextInstruction.RECORD_VALUE || instruction == ToTextInstruction.RECORD_LAST_VALUE) {
          var label = recordLabels.pop();
          stringBuilder.append(label);
          stringBuilder.append(" = ");
        }

        // then, print the value
        var value = values.pop();

        if (value instanceof Long) {
          stringBuilder.append((Long) value);
        } else if (value instanceof Double) {
          stringBuilder.append((Double) value);
        } else if (value instanceof String) {
          stringBuilder.append("\"");
          stringBuilder.append(StringEscapeUtils.escapeJava((String) value));
          stringBuilder.append("\"");
        } else if (value instanceof Boolean) {
          stringBuilder.append((Boolean) value);
        } else if (value instanceof Record) {
          HashMap<String, Object> rec = ((Record) value).getElements();
          // if the record is empty, just emit it
          if (rec.isEmpty()) {
            stringBuilder.append("{}");
          } else {
            stringBuilder.append("{");
            instructions.push(ToTextInstruction.RECORD_END);
            String[] keys = rec.keySet().toArray(new String[0]);
            Arrays.sort(keys); // stable printing
            var last_key = keys[keys.length - 1];
            instructions.push(ToTextInstruction.RECORD_LAST_VALUE);
            recordLabels.push(last_key);
            values.push(rec.get(last_key));
            for (int i = keys.length - 2; i >= 0; i--) {
              instructions.push(ToTextInstruction.RECORD_VALUE);
              recordLabels.push(keys[i]);
              values.push(rec.get(keys[i]));
            }
          }
        } else {
          throw new PrimOpError("Expected value in toText(), but got " + value.getClass());
        }

        // if it's a record value, but _not_ the last one, print the comma
        if (instruction == ToTextInstruction.RECORD_VALUE) {
          stringBuilder.append(", ");
        }
      }

      if (instruction == ToTextInstruction.RECORD_END) {
        stringBuilder.append("}");
      }
    }

    return stringBuilder.toString();
  }
}
