package org.katlas.JavaKh;
import java.math.BigInteger;
// rationals
// using BigIntegers since they are probably necessary
public class Rational extends BaseRing {
    BigInteger n, d;
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

    public BaseRing inverse() {
	return new Rational(d, n);
    }

    public BaseRing multiply(BaseRing br) {
	Rational r = (Rational) br;
	return new Rational(n.multiply(r.n), d.multiply(r.d));
    }

    public BaseRing multiply(int n) {
	return new Rational(this.n.multiply(BigInteger.valueOf(n)), d);
    }

    public BaseRing add(BaseRing br) {
	Rational r = (Rational) br;
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
}
