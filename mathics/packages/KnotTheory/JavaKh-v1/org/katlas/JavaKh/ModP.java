package org.katlas.JavaKh;
public class ModP extends BaseRing {
    static int p, inv[];
    int n;

    public static void setP(int i) {
	p = i;
	inv = new int[p];
	for (int j = 1; j < p; j++)
	    for (int k = 1; k < p; k++)
		if ((j * k) % p == 1) {
		    inv[j] = k;
		    break;
		}
    }

    public ModP(int i) {
	n = i % p;
	if (n < 0)
	    n += p;
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

    public BaseRing inverse() {
	assert inv[n] != 0;
	return new ModP(inv[n]);
    }

    public BaseRing multiply(BaseRing br) {
	ModP mp = (ModP) br;
	return new ModP(n * mp.n);
    }

    public BaseRing multiply(int n) {
	return new ModP(this.n * n);
    }

    public BaseRing add(BaseRing br) {
	ModP mp = (ModP) br;
	return new ModP(n + mp.n);
    }

    public boolean isZero() {
	return n == 0;
    }

    public String toString() {
	return Integer.toString(n);
    }
}
