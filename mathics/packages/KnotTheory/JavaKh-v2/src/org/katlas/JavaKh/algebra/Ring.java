package org.katlas.JavaKh.algebra;

public interface Ring<R extends Ring<R>> {

	R add(R r);
	R multiply(R r);
	R multiply(int r);

	boolean isInvertible();
	R inverse();
	
	boolean isZero();	
}
