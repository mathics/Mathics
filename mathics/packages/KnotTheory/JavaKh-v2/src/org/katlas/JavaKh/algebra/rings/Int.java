package org.katlas.JavaKh.algebra.rings;
import java.math.BigInteger;

import org.katlas.JavaKh.algebra.Ring;

public class Int implements Ring<Int> {
	private static final BigInteger MINUSONE = BigInteger.valueOf(-1);
	
    final private BigInteger n;

    public BigInteger getN() {
		return n;
	}

	public Int(int i) {
	n = BigInteger.valueOf(i);
    }

    public Int(BigInteger i) {
	n = i;
    }

    public boolean equals(Object o) {
	if (!(o instanceof Int))
	    return false;
	Int i = (Int) o;
	return n.equals(i.n);
    }

    public boolean isInvertible() {
	return n.equals(BigInteger.ONE) || n.equals(MINUSONE);
    }

    public Int inverse() {
	assert isInvertible();
	return this;
    }

    public Int multiply(Int i) {
	return new Int(n.multiply(i.n));
    }

    public Int multiply(int n) {
	return new Int(this.n.multiply(BigInteger.valueOf(n)));
    }

    public Int add(Int i) {
	return new Int(n.add(i.n));
    }

    public boolean isZero() {
	return n.equals(BigInteger.ZERO);
    }

    public String toString() {
	return n.toString();
    }
}
