package org.katlas.JavaKh.algebra.rings;

import java.io.Serializable;

import org.katlas.JavaKh.algebra.Ring;

public class ModP implements Ring<ModP>, Serializable {
	/**
	 * 
	 */
	private static final long serialVersionUID = 6755912305388859426L;
	static int p, inv[];
	static ModP[] values;
	final int n;

	public static void setP(int i) {
		p = i;
		inv = new int[p];
		for (int j = 1; j < p; j++) {
			for (int k = 1; k < p; k++) {
				if ((j * k) % p == 1) {
					inv[j] = k;
					break;
				}
			}
		}
		values = new ModP[p];
		for (int j = 0; j < p; j++) {
			values[j] = new ModP(j);
		}
	}

	public ModP(int i) {
		n = mod(i);
	}

	private int mod(int i) {
		assert p != 0;
		i = i % p;
		if(i < 0) i+= p;
		return i;
	}

	public boolean equals(Object o) {
		if (!(o instanceof ModP))
			return false;
		ModP mp = (ModP) o;
		return n == mp.n;
	}

	public boolean isInvertible() {
		return n != 0;
	}

	public ModP inverse() {
		assert inv[n] != 0;
		return values[inv[n]];
	}

	public ModP multiply(ModP mp) {
		return values[mod(n * mp.n)];
	}

	public ModP multiply(int n) {
		return values[mod(this.n * n)];
	}

	public ModP add(ModP mp) {
		return values[mod(n + mp.n)];
	}

	public boolean isZero() {
		return n == 0;
	}

	public String toString() {
		return Integer.toString(n);
	}
}
