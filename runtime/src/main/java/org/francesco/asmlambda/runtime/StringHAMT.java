package org.francesco.asmlambda.runtime;

import java.util.ArrayList;

/** An Hash Array Mapped Trie with Text as keys, and {@link Object}s as values. */
public class StringHAMT {
  // ------------------------------------------------------------------
  // bit twiddling

  private static int BITS_PER_SUBKEY = 4;
  private static int SUBKEY_MASK = (1 << BITS_PER_SUBKEY) - 1; // 1111

  // Masks out the BITS_PER_SUBKEY at this shift level:
  //
  // index(0100 1011 0011 0100, 7)
  //    ==>
  //       0000 0000 0000 0110
  //
  // This will give the absolute index for the provided hash
  // in the array used for each branch of the HAMT
  private static int index(int hash, int shift) {
    return (hash >>> shift) & SUBKEY_MASK;
  }

  // This gives the _bit_ corresponding to the index at the given
  // shift for the given hash. This is needed to know what part of
  // the branch bitmap to consider.
  //
  // mask(0100 1011 0011 0100, 7)
  //   ==>
  // 1 << index(0100 1011 0011 0100, 7)
  //   ==>
  // 1 << 0000 0000 0000 0110
  //   ==>
  // 1 << 6
  //   ==>
  // 0000 0000 0010 0000
  private static int mask(int hash, int shift) {
    return 1 << StringHAMT.index(hash, shift);
  }

  // This gives the index into the current array of the branch, based
  // on the bitmap.
  //
  // sparseIndex(0100 1101 1000 1101, mask(0100 1011 0011 0100, 7))
  //   ==>
  // sparseIndex(0100 1101 1000 1101, 0000 0000 0010 0000)
  //   ==>
  // Integer.bitCount(0100 1101 1000 1101 & (0000 0000 0010 0000 - 1))
  //   ==>
  // Integer.bitCount(0100 1101 1000 1101 & 0000 0000 0001 1111)
  //   ==>
  // Integer.bitCount(0000 0000 0000 1101)
  //   ==>
  // 4
  private static int sparseIndex(int bitmap, int mask) {
    return Integer.bitCount(bitmap & (mask - 1));
  }

  // ------------------------------------------------------------------
  // nodes
  //
  // We use recursion here since the maximum height is 7 anyway, so it
  // does not matter.

  private interface Node {
    Object get(int hash, int shift, String k);
    Node put(int hash, int shift, String k, Object v);
    Node remove(int hash, int shift, String k);
    void keys(ArrayList<String> keys);
    long size();

    static Object get(Node node, String k) {
      if (node == null) {
        return null;
      } else {
        return node.get(k.hashCode(), 0, k);
      }
    }

    static Node put(Node node, String k, Object v) {
      int hash = k.hashCode();
      if (node == null) {
        return new Leaf(hash, k, v);
      } else {
        return node.put(hash, 0, k, v);
      }
    }

    static Node remove(Node node, String k) {
      if (node == null) {
        return null;
      } else {
        return node.remove(k.hashCode(), 0, k);
      }
    }

    static String[] keys(Node node) {
      if (node == null) {
        return new String[0];
      } else {
        ArrayList<String> keys = new ArrayList<>();
        node.keys(keys);
        return keys.toArray(new String[0]);
      }
    }

    static boolean isLeafOrCollision(Node node) {
      return node instanceof Leaf || node instanceof Collision;
    }

    static long size(Node node) {
      if (node == null) {
        return 0;
      } else {
        return node.size();
      }
    }
  }

  private static class Branch implements Node {
    int bitmap;
    Node[] buckets;

    Branch(int bitmap, Node[] buckets) {
      this.bitmap = bitmap;
      this.buckets = buckets;
    }

    @Override
    public Object get(int hash, int shift, String k) {
      int mask = StringHAMT.mask(hash, shift);
      if ((this.bitmap & mask) == 0) {
        return null;
      }
      return this.buckets[StringHAMT.sparseIndex(this.bitmap, mask)]
          .get(hash, shift + StringHAMT.BITS_PER_SUBKEY, k);
    }

    @Override
    public Node put(int hash, int shift, String k, Object v) {
      int mask = StringHAMT.mask(hash, shift);
      int numBuckets = this.buckets.length;
      if ((this.bitmap & mask) == 0) {
        int ix = StringHAMT.sparseIndex(this.bitmap, mask);
        Node[] buckets = new Node[numBuckets + 1];
        System.arraycopy(this.buckets, 0, buckets, 0, ix);
        buckets[ix] = new Leaf(hash, k, v);
        System.arraycopy(this.buckets, ix, buckets, ix + 1, numBuckets - ix);
        return new Branch(this.bitmap | mask, buckets);
      } else {
        Node[] buckets = new Node[this.buckets.length];
        System.arraycopy(this.buckets, 0, buckets, 0, numBuckets);
        Node node = this.buckets[StringHAMT.sparseIndex(this.bitmap, mask)]
            .put(hash, shift + StringHAMT.BITS_PER_SUBKEY, k, v);
        buckets[StringHAMT.sparseIndex(this.bitmap, mask)] = node;
        return new Branch(this.bitmap, buckets);
      }
    }

    @Override
    public Node remove(int hash, int shift, String k) {
      int mask = StringHAMT.mask(hash, shift);
      if ((this.bitmap & mask) == 0) {
        return this;
      } else {
        int ix = StringHAMT.sparseIndex(this.bitmap, mask);
        int numBuckets = this.buckets.length;
        Node node = this.buckets[ix].remove(hash, shift + StringHAMT.BITS_PER_SUBKEY, k);
        if (node == null) {
          if (numBuckets == 1) {
            return null;
          } else if (numBuckets == 2) {
            if (ix == 0 && Node.isLeafOrCollision(this.buckets[1])) {
              return this.buckets[1];
            } else if (ix == 1 && Node.isLeafOrCollision(this.buckets[0])) {
              return this.buckets[0];
            }
          }
          Node[] buckets = new Node[numBuckets - 1];
          System.arraycopy(this.buckets, 0, buckets, 0, ix);
          System.arraycopy(this.buckets, ix + 1, buckets, ix, numBuckets - ix - 1);
          return new Branch(this.bitmap & ~mask, buckets);
        } else {
          Node[] buckets = new Node[numBuckets];
          System.arraycopy(this.buckets, 0, buckets, 0, numBuckets);
          buckets[ix] = node;
          return new Branch(this.bitmap, buckets);
        }
      }
    }

    @Override
    public void keys(ArrayList<String> keys) {
      for (Node node : this.buckets) {
        node.keys(keys);
      }
    }

    @Override
    public long size() {
      var size = 0;
      for (Node node : buckets) {
        size += node.size();
      }
      return size;
    }
  }

  private static class Leaf implements Node {
    int hash;
    String k;
    Object v;

    Leaf(int hash, String k, Object v) {
      this.hash = hash;
      this.k = k;
      this.v = v;
    }

    @Override
    public Object get(int hash, int shift, String k) {
      if (this.hash == hash) {
        return this.v;
      } else {
        return null;
      }
    }

    @Override
    public Node put(int hash, int shift, String k, Object v) {
      if (this.hash == hash) {
        if (this.k.equals(k)) {
          return this;
        } else {
          return new Collision(hash, this.k, this.v, k, v);
        }
      } else {
        Node branch = new Branch(0, new Node[0]);
        branch = branch.put(this.hash, shift, this.k, this.v);
        branch = branch.put(hash, shift, k, v);
        return branch;
      }
    }

    @Override
    public Node remove(int hash, int shift, String k) {
      if (this.hash == hash && this.k.equals(k)) {
        return null;
      } else {
        return this;
      }
    }

    @Override
    public void keys(ArrayList<String> keys) {
      keys.add(this.k);
    }

    @Override
    public long size() {
      return 1;
    }
  }

  private static class CollisionEntry {
    String k;
    Object v;

    CollisionEntry(String k, Object v) {
      this.k = k;
      this.v = v;
    }
  }

  private static class Collision implements Node {
    int hash;
    CollisionEntry[] entries;

    Collision(int hash, CollisionEntry[] entries) {
      this.hash = hash;
      this.entries = entries;
    }

    Collision(int hash, String k1, Object v1, String k2, Object v2) {
      this.hash = hash;
      this.entries = new CollisionEntry[2];
      this.entries[0] = new CollisionEntry(k1, v1);
      this.entries[1] = new CollisionEntry(k2, v2);
    }

    @Override
    public Object get(int hash, int shift, String k) {
      if (this.hash == hash) {
        for (CollisionEntry entry : this.entries) {
          if (entry.k.equals(k)) {
            return entry.v;
          }
        }
      }
      return null;
    }

    @Override
    public Node put(int hash, int shift, String k, Object v) {
      if (this.hash == hash) {
        CollisionEntry[] entries = new CollisionEntry[this.entries.length + 1];
        System.arraycopy(this.entries, 0, entries, 0, this.entries.length);
        entries[this.entries.length] = new CollisionEntry(k, v);
        return new Collision(hash, entries);
      } else {
        Node[] buckets = new Node[1];
        buckets[0] = this;
        Node branch = new Branch(StringHAMT.mask(this.hash, shift), buckets);
        return branch.put(hash, shift, k, v);
      }
    }

    @Override
    public Node remove(int hash, int shift, String k) {
      if (this.hash == hash) {
        int found = -1;
        int numEntries = this.entries.length;
        for (int ix = 0; ix < numEntries; ix++) {
          if (this.entries[ix].k.equals(k)) {
            found = ix;
            break;
          }
        }
        if (found >= 0) {
          CollisionEntry[] entries = new CollisionEntry[numEntries - 1];
          System.arraycopy(this.entries, 0, entries, 0, found);
          System.arraycopy(this.entries, found, entries, found + 1, numEntries - found - 1);
          return new Collision(hash, entries);
        } else {
          return this;
        }
      } else {
        return this;
      }
    }

    @Override
    public void keys(ArrayList<String> keys) {
      for (CollisionEntry entry : this.entries) {
        keys.add(entry.k);
      }
    }

    @Override
    public long size() {
      return this.entries.length;
    }
  }

  private Node root;

  public StringHAMT() {
    root = null;
  }

  private StringHAMT(Node root) {
    this.root = root;
  }

  public StringHAMT put(String k, Object v) {
    return new StringHAMT(Node.put(this.root, k, v));
  }

  public Object get(String k) {
    return Node.get(this.root, k);
  }

  public StringHAMT remove(String k) {
    return new StringHAMT(Node.remove(this.root, k));
  }

  public String[] keys() {
    return Node.keys(this.root);
  }

  public long size() {
    return Node.size(this.root);
  }
}
