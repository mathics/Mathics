package org.katlas.JavaKh;
public class Cap implements Comparable {
    public int n, ncycles;
    public int pairings[];
    private static java.util.Map cache = new java.util.TreeMap();

    public Cap(int n, int cycles) {
	this.n = n;
	ncycles = cycles;
	pairings = new int[n];
	// let pairings be filled elsewhere
    }

    public boolean equals(Object o) {
	if (!(o instanceof Cap))
	    return false;
	Cap c = (Cap) o;
	if (c.n != n)
	    return false;
	if (ncycles != c.ncycles)
	    return false;
	if (!java.util.Arrays.equals(pairings, c.pairings))
	    return false;
	return true;
    }

    public int compareTo(Object o) {
	Cap c = (Cap) o;
	if (n != c.n)
	    return n - c.n;
	if (ncycles != c.ncycles)
	    return ncycles - c.ncycles;
	for (int i = 0; i < n; i++)
	    if (pairings[i] != c.pairings[i])
		return pairings[i] - c.pairings[i];
	return 0;
    }

    public Cap compose(int start, Cap c, int cstart, int nc) {
	ComposeInput ci = new ComposeInput(this, start, c, cstart, nc);
	ComposeOutput co = (ComposeOutput) cache.get(ci);
	if (co == null) {
	    co = new ComposeOutput(ci);
	    cache.put(ci, co);
	}
	return co.cap;
    }

    public Cap compose(int start, Cap c, int cstart, int nc,
		       int joins[]) {
	ComposeInput ci = new ComposeInput(this, start, c, cstart, nc);
	ComposeOutput co = (ComposeOutput) cache.get(ci);
	if (co == null) {
	    co = new ComposeOutput(ci);
	    cache.put(ci, co);
	}
	System.arraycopy(co.joins, 0, joins, 0, co.joins.length);
	return co.cap;
    }

    // horizontal composition
    public Cap compose2(int start, Cap c, int cstart, int nc,
		       int joins[]) {
	// joins is to be filled with info about new cycles
	Cap ret = new Cap(n + c.n - 2 * nc, ncycles + c.ncycles);
	// start labelling the edges with the one following the join on this
	for (int i = 0; i < n - nc; i++) {
	    int ii = (i + start + nc) % n;
	    ret.pairings[i] = (pairings[ii] - start - nc + 2 * n) % n;
	    // do something different if it pairs with a connecting edge:
	    if (ret.pairings[i] >= n - nc)
		ret.pairings[i] = -1 - (ret.pairings[i] - n + nc);
	}
	int thisjoins[] = new int[nc];
	for (int i = 0; i < nc; i++) {
	    int ii = (i + start) % n;
	    thisjoins[i] = (pairings[ii] - start - nc + 2 * n) % n;
	    if (thisjoins[i] >= n - nc)
		thisjoins[i] = -1 - (thisjoins[i] - n + nc);
	}
	for (int i = 0; i < c.n - nc; i++) {
	    int ii = (i + cstart + nc) % c.n;
	    int j = i + n - nc;
	    ret.pairings[j] = (c.pairings[ii] - cstart - nc + 2 * c.n) % c.n;
	    if (ret.pairings[j] >= c.n - nc)
		ret.pairings[j] = -1 - (c.n - ret.pairings[j] - 1);
	    else
		ret.pairings[j] += n - nc;
	}
	int cjoins[] = new int[nc];
	for (int i = 0; i < nc; i++) {
	    int ii = (cstart + nc - 1 - i + c.n) % c.n;
	    cjoins[i] = (c.pairings[ii] - cstart - nc + 2 * c.n) % c.n;
	    if (cjoins[i] >= c.n - nc)
		cjoins[i] = -1 - (c.n - cjoins[i] - 1);
	    else
		cjoins[i] += n - nc;
	}
	boolean joinsdone[] = new boolean[nc];
	java.util.Arrays.fill(joinsdone, false);
	for (int i = 0; i < n - nc; i++)
	    if (ret.pairings[i] < 0) {
		while (ret.pairings[i] < 0) {
		    int j = -1 - ret.pairings[i];
		    joinsdone[j] = true;
		    ret.pairings[i] = cjoins[j];
		    if (ret.pairings[i] < 0) {
			j = -1 - ret.pairings[i];
			joinsdone[j] = true;
			ret.pairings[i] = thisjoins[j];
		    }
		}
		ret.pairings[ret.pairings[i]] = i;
	    }
	for (int i = n - nc; i < ret.n; i++)
	    if (ret.pairings[i] < 0) {
		while (ret.pairings[i] < 0) {
		    int j = -1 - ret.pairings[i];
		    joinsdone[j] = true;
		    ret.pairings[i] = thisjoins[j];
		    if (ret.pairings[i] < 0) {
			j = -1 - ret.pairings[i];
			joinsdone[j] = true;
			ret.pairings[i] = cjoins[j];
		    }
		}
		ret.pairings[ret.pairings[i]] = i;
	    }

	for (int i = 0; i < nc; i++)
	    if (joinsdone[i])
		joins[i] = -1;
	// finally, handle any newly created cycles
	int nnew = 0;
	for (int i = 0; i < nc; i++)
	    if (!joinsdone[i]) {
		int j = i;
		do {
		    joinsdone[j] = true;
		    joins[j] = nnew;
		    j = -1 - thisjoins[j];
		    joinsdone[j] = true;
		    joins[j] = nnew;
		    j = -1 - cjoins[j];
		} while (j != i);
		ret.ncycles++;
		nnew++;
	    }
	return ret;
    }

    private static int generateHelper(Cap caps[], int ncaps, int n,
				      int pairings[], int p) {
	if (p == n) {
	    caps[ncaps] = new Cap(n, (int) (Math.random() * 5));
	    for (int i = 0; i < n; i++)
		caps[ncaps].pairings[i] = pairings[i];
	    return ncaps + 1;
	}
	for (int i = 0; i < n; i++) {
	    if (pairings[i] == -1) {
		int max;
		for (max = i + 1; max < n && pairings[max] == -1; max++);
		for (int j = i + 1; j < max; j += 2) {
		    pairings[i] = j;
		    pairings[j] = i;
		    ncaps = generateHelper(caps, ncaps, n, pairings, p + 2);
		    pairings[i] = -1;
		    pairings[j] = -1;
		}
		return ncaps;
	    }
	}
	return -1;
    }

    public static Cap[] generate(int n) { // generates all Caps with n points
	int pairings[] = new int[n];
	for (int i = 0; i < n; i++)
	    pairings[i] = -1;
	int ncaps = 1, k = n / 2;
	for (int i = k + 2; i <= n; i++) // ncaps = (2k)!/(k!(k+1)!)
	    ncaps *= i;
	for (int i = 2; i <= k; i++)
	    ncaps /= i;
	Cap caps[] = new Cap[ncaps];
	generateHelper(caps, 0, n, pairings, 0);
	return caps;
    }

    // tests horizontal composition associativity
    public static void main(String[] args) {
	Cap caps[] = generate(6);
	int joins[] = new int[10]; // dummy
	for (int i = 0; i < caps.length; i++)
	    for (int j = 0; j < caps.length; j++)
		for (int k = 0; k < caps.length; k++) {
		    Cap a = caps[i].compose(1, caps[j], 4, 2, joins).compose(5, caps[k], 3, 3, joins);
		    Cap b = caps[i].compose(1, caps[j].compose(1, caps[k], 3, 3, joins), 0, 2, joins);
		    if (!a.equals(b)) {
			System.out.println("Error in associativity check");
			return;
		    }
		}
	System.out.println("Associativity checks OK");
    }

    private class ComposeInput implements Comparable {
	Cap a, b;
	int astart, bstart, nc;
	public ComposeInput(Cap a, int astart, Cap b, int bstart, int nc) {
	    this.a = a;
	    this.b = b;
	    this.astart = astart;
	    this.bstart = bstart;
	    this.nc = nc;
	}
	public int compareTo(Object o) {
	    ComposeInput ci = (ComposeInput) o;
	    if (astart != ci.astart)
		return astart - ci.astart;
	    if (bstart != ci.bstart)
		return bstart - ci.bstart;
	    if (nc != ci.nc)
		return nc - ci.nc;
	    int i = a.compareTo(ci.a);
	    if (i != 0)
		return i;
	    return b.compareTo(ci.b);
	}
    }

    private class ComposeOutput {
	Cap cap;
	int joins[];
	public ComposeOutput(ComposeInput ci) {
	    joins = new int[ci.nc];
	    cap = ci.a.compose2(ci.astart, ci.b, ci.bstart, ci.nc, joins);
	}
    }
}
