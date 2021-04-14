package org.katlas.JavaKh.algebra.rings;
import java.io.Serializable;
import java.math.BigInteger;

import org.katlas.JavaKh.algebra.Ring;

// rationals
// using BigIntegers since they are probably necessary
public class Rational implements Ring<Rational>, Serializable {
    /**
	 * 
	 */
	private static final long serialVersionUID = -6075252910094756294L;
	final BigInteger n, d;
    public Rational(int n) {
	this.n = BigInteger.valueOf(n);
	d = BigInteger.ONE;
    }

    private Rational(BigInteger n, BigInteger d) {
	BigInteger gcd = n.gcd(d);
	this.n = n.divide(gcd);
	this.d = d.divide(gcd);
    }

    public boolean isInvertible() {
	return !(n.equals(BigInteger.ZERO));
    }

    public Rational inverse() {
	return new Rational(d, n);
    }

    public Rational multiply(Rational r) {
	return new Rational(n.multiply(r.n), d.multiply(r.d));
    }

    public Rational multiply(int n) {
	return new Rational(this.n.multiply(BigInteger.valueOf(n)), d);
    }

    public Rational add(Rational r) {
	return new Rational(n.multiply(r.d).add(r.n.multiply(d)),
			    d.multiply(r.d));
    }

    public boolean isZero() {
	return n.equals(BigInteger.ZERO);
    }

    public boolean equals(Object o) {
	if (!(o instanceof Rational))
	    return false;
	Rational r = (Rational) o;
	return n.multiply(r.d).equals(r.n.multiply(d));
    }

	@Override
	public String toString() {
		return n.toString() + "/" + d.toString();
	}
    
    
}
