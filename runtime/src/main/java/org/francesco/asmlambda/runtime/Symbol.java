package org.francesco.asmlambda.runtime;

import java.util.HashMap;

public final class Symbol {
  public int token;
  public String string;

  private Symbol() {}

  private Symbol(int token, String string) {
    this.token = token;
    this.string = string;
  }

  // we are forced to override equals and hashCode because they are used in HashMap and we do not want to wrap symbols.

  @Override
  public boolean equals(Object that) {
    if (!(that instanceof Symbol)) {
      return false;
    }
    return token == ((Symbol) that).token;
  }

  @Override
  public int hashCode() {
    return token;
  }

  @Override
  public String toString() {
    return ":" + this.string;
  }

  // interning
  private static int current_token = 0;
  private static HashMap<String, Symbol> interned = new HashMap<>();

  public Symbol intern(String symString) {
    Symbol sym = interned.get(symString);
    if (sym == null) {
      sym = new Symbol(current_token, symString);
      interned.put(symString, sym);
      current_token++;
    }
    return sym;
  }
}
