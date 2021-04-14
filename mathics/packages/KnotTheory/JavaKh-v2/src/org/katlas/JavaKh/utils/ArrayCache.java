package org.katlas.JavaKh.utils;

import gnu.trove.TObjectHashingStrategy;

import java.util.Arrays;

public class ArrayCache extends HashCodeCache<byte[]> implements Cache<byte[]> {

  public ArrayCache() {
    super(new ArrayHashingStrategy());
  }

  private static class ArrayHashingStrategy implements TObjectHashingStrategy<byte[]> {
    private static final long serialVersionUID = 746582354L;

    public int computeHashCode(byte[] array) {
      return Arrays.hashCode(array);
    }

    public boolean equals(byte[] o1, byte[] o2) {
      return Arrays.equals(o1, o2);
    }
  }

}
