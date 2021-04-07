package org.katlas.JavaKh.utils;

import gnu.trove.TObjectHashingStrategy;

import java.util.Arrays;

public class DoubleArrayCache extends HashCodeCache<byte[][]> implements Cache<byte[][]> {

  public DoubleArrayCache() {
    super(new DoubleArrayHashingStrategy());
  }

  private static class DoubleArrayHashingStrategy implements TObjectHashingStrategy<byte[][]> {
    private static final long serialVersionUID = 746582354L;

    public int computeHashCode(byte[][] array) {
      return Arrays.deepHashCode(array);
    }

    public boolean equals(byte[][] o1, byte[][] o2) {
      return Arrays.deepEquals(o1, o2);
    }
  }

}
