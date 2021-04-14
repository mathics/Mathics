package org.katlas.JavaKh;
import java.util.*;

public class LCCC { // Linear Combination of Canned Cobordisms
    //static int maxsz = 0; // is 10 for T(7.6)
    Cap top, bottom;
    // coefficients are BaseRing
    int n;
    CannedCobordism cobordisms[];
    BaseRing coefficients[];
    // consider storing the hash codes?

    public LCCC(Cap t, Cap b) {
	//if (t == null || b == null)
	//throw new IllegalArgumentException();
	top = t;
	bottom = b;
	n = 0;
	cobordisms = new CannedCobordism[8];
	coefficients = new BaseRing[8];
    }

    public boolean equals(Object o) {
	if (o == null && n == 0)
	    return true;
	if (!(o instanceof LCCC))
	    return false;
	LCCC other = (LCCC) o;
	if (n > 1 || other.n > 1)
	    // not working right now
	    throw new UnsupportedOperationException();
	if (n == 0) {
	    if (other.n != 0 && !other.coefficients[0].isZero())
		return false;
	} else if (other.n == 0 && !coefficients[0].isZero())
	    return false;
	else if (!(cobordisms[0].equals(other.cobordisms[0])
		   && coefficients[0].equals(other.coefficients[0])))
	    return false;
	return true;
    }

    public static void main(String args[]) {
	// associativity check
	Cap caps[] = Cap.generate(10);
	for (int i = 0; i < caps.length; i++)
	    for (int j = 0; j < caps.length; j++)
		for (int k = 0; k < caps.length; k++)
		    for (int l = 0; l < caps.length; l++) {
			CannedCobordism
			    a1=CannedCobordism.generateRandom(caps[i],caps[j]),
			    a2=CannedCobordism.generateRandom(caps[i],caps[j]),
			    a3=CannedCobordism.generateRandom(caps[i],caps[j]),
			    b1=CannedCobordism.generateRandom(caps[j],caps[k]),
			    b2=CannedCobordism.generateRandom(caps[j],caps[k]),
			    b3=CannedCobordism.generateRandom(caps[j],caps[k]),
			    c1=CannedCobordism.generateRandom(caps[k],caps[l]),
			    c2=CannedCobordism.generateRandom(caps[k],caps[l]),
			    c3=CannedCobordism.generateRandom(caps[k],caps[l]);
			LCCC a = new LCCC(caps[i], caps[j]);
			a.add(a1, (int) (Math.random() * 10));
			a.add(a2, (int) (Math.random() * 10));
			a.add(a3, (int) (Math.random() * 10));
			LCCC b = new LCCC(caps[j], caps[k]);
			b.add(b1, (int) (Math.random() * 10));
			b.add(b2, (int) (Math.random() * 10));
			b.add(b3, (int) (Math.random() * 10));
			LCCC c = new LCCC(caps[k], caps[l]);
			c.add(c1, (int) (Math.random() * 10));
			c.add(c2, (int) (Math.random() * 10));
			c.add(c3, (int) (Math.random() * 10));
			LCCC d = (c.compose(b)).compose(a);
			LCCC e = c.compose(b.compose(a));
			if (!d.equals(e)) {
			    System.out.println("Error in associativity check");
			    return;
			}
			LCCC f = c.reduce().compose(b.reduce().compose(a.reduce()));
			if (!f.reduce().equals(e.reduce())) {
			    System.out.println("Error in reduction check");
			    return;
			}
		    }
	System.out.println("Associativity checks OK!");
    }

    public void add(CannedCobordism cc, int n) {
	add(cc, BaseRing.fromInt(n));
    }

    public void add(CannedCobordism cc, BaseRing num) {
	for (int i = 0; i < n; i++)
	    if (cobordisms[i].equals(cc)) {
		coefficients[i] = coefficients[i].add(num);
		if (coefficients[i].isZero()) {
		    cobordisms[i] = cobordisms[n - 1];
		    coefficients[i] = coefficients[n - 1];
		    n--;
		}
		return;
	    }
	if (num.isZero())
	    return;
	if (n == coefficients.length) {
	    CannedCobordism newccs[] = new CannedCobordism[2 * coefficients.length];
	    System.arraycopy(cobordisms, 0, newccs, 0, n);
	    cobordisms = newccs;
	    BaseRing newbrs[] = new BaseRing[2 * coefficients.length];
	    System.arraycopy(coefficients, 0, newbrs, 0, n);
	    coefficients = newbrs;
	}
	cobordisms[n] = cc;
	coefficients[n] = num;
	n++;
	// DEBUG
	/*if (entries.size() > maxsz)
	  maxsz = entries.size();*/
    }

    public void add(LCCC other) {
	if (other == null)
	    return;
	for (int i = 0; i < other.n; i++)
	    add(other.cobordisms[i], other.coefficients[i]);
    }

    public void multiply(BaseRing num) {
	if (num.isZero())
	    n = 0;
	else
	    for (int i = 0; i < n; i++)
		coefficients[i] = coefficients[i].multiply(num);
    }

    public LCCC compose(LCCC other) { // vertical composition
	if (other == null || n == 0 || other.n == 0)
	    return null;
	assert top.equals(other.bottom);
	LCCC ret = new LCCC(other.top, bottom);
	for (int i = 0; i < n; i++)
	    for (int j = 0; j < other.n; j++)
		ret.add(cobordisms[i].compose(other.cobordisms[j]),
			coefficients[i].multiply(other.coefficients[j]));
	return ret;
    }

    // horizontal composition
    public LCCC compose(int start, CannedCobordism cc, int cstart, int nc,
			boolean reverse) {
	if (n == 0)
	    return null;
	LCCC ret = new LCCC(null, null);
	if (reverse)
	    for (int i = 0; i < n; i++)
		ret.add(cc.compose(cstart, cobordisms[i], start, nc),
			coefficients[i]);
	else
	    for (int i = 0; i < n; i++)
		ret.add(cobordisms[i].compose(start, cc, cstart, nc),
			coefficients[i]);
	if (ret.n == 0)
	    return null;
	else {
	    ret.top = ret.cobordisms[0].top;
	    ret.bottom = ret.cobordisms[0].bottom;
	    return ret;
	}
    }

    public LCCC reduce() {
	if (JavaKh.using_h)
	    return reduceWithH();
	if (n == 0)
	    return null;
	LCCC ret = new LCCC(top, bottom);
	for (int iter = 0; iter < n; iter++) {
	    CannedCobordism cc = cobordisms[iter];
	    cc.reverseMaps();
	    BaseRing num = coefficients[iter];
	    int dots[] = new int[cc.nbc];
	    int genus[] = CannedCobordism.zeros[cc.nbc];
	    int moreWork[] = new int[cc.ncc];
	    int nmoreWork = 0;
	    boolean kill = false;
	    for (int i = 0; i < cc.ncc; i++)
		if (cc.genus[i] + cc.dots[i] > 1) {
		    kill = true;
		} else if (cc.boundaryComponents[i].length == 0) {
		    if (cc.genus[i] == 1)
			num = num.multiply(2);
		    else if (cc.dots[i] == 0)
			kill = true;
		} else if (cc.boundaryComponents[i].length == 1) {
		    dots[cc.boundaryComponents[i][0]] = cc.dots[i]+cc.genus[i];
		    if (cc.genus[i] == 1)
			num = num.multiply(2);
		} else {
		    // neck cutting relation
		    if (cc.genus[i] + cc.dots[i] == 1) { // only one choice
			if (cc.genus[i] == 1) {
			    // there is a 2 from the torus
			    num = num.multiply(2);
			}
			// use dots to cancel out the other factors
			for (int j =0; j<cc.boundaryComponents[i].length;j++)
			    dots[cc.boundaryComponents[i][j]] = 1;
		    } else {
			// cc.bC[i].length choices
			// use dots to cancel out all the factors, except
			// one which is cancelled by the torus
			// do these last
			moreWork[nmoreWork++] = i;
		    }
		}
	    if (kill)
		continue;
	    int neckCutting[][] = new int[1][];
	    neckCutting[0] = dots;
	    for (int i = 0; i < nmoreWork; i++) {
		int concomp = moreWork[i];
		int nbc = cc.boundaryComponents[concomp].length;
		int newarr[][] = new int[neckCutting.length * nbc][cc.nbc];
		for (int j = 0; j < neckCutting.length; j++) {
		    System.arraycopy(neckCutting[j], 0, newarr[j * nbc], 0,
				     cc.nbc);
		    for (int k = 0; k < nbc; k++)
			newarr[j * nbc][cc.boundaryComponents[concomp][k]] = 1;
		    for (int k = 1; k < nbc; k++)
			System.arraycopy(newarr[j * nbc], 0,
					 newarr[j * nbc + k], 0, cc.nbc);
		    for (int k = 0; k < nbc; k++)
			newarr[j * nbc + k][cc.boundaryComponents[concomp][k]] = 0;
		}
		neckCutting = newarr;
	    }
	    int connectedComponent[] = CannedCobordism.counting[cc.nbc];
	    for (int i = 0; i < neckCutting.length; i++) {
		CannedCobordism newcc = new CannedCobordism(top, bottom);
		// IMPORTANT!!! in order for them to safely share arrays
		// CannedCobordisms must be treated as immutable
		newcc.connectedComponent = connectedComponent;
		newcc.ncc = newcc.nbc;
		newcc.genus = genus;
		newcc.dots = neckCutting[i];
		ret.add(newcc, num);
	    }
	}
	if (ret.n == 0)
	    return null;
	else
	    return ret;
    }

    public LCCC reduceWithH() {
	if (n == 0)
	    return null;
	LCCC ret = new LCCC(top, bottom);
	for (int iter = 0; iter < n; iter++) {
	    CannedCobordism cc = cobordisms[iter];
	    cc.reverseMaps();
	    BaseRing num = coefficients[iter];
	    int dots[] = new int[cc.nbc];
	    int hpow = cc.hpower;
	    int moreWork[] = new int[cc.ncc];
	    int nmoreWork = 0;
	    boolean kill = false;
	    for (int i = 0; i < cc.ncc; i++)
		if (cc.boundaryComponents[i].length == 0) {
		    if (cc.dots[i] > 0)
			hpow += cc.dots[i] + cc.genus[i] - 1;
		    else if (cc.genus[i] % 2 == 0)
			kill = true;
		    else {
			num = num.multiply(2);
			hpow += cc.genus[i] - 1;
		    }
		} else if (cc.boundaryComponents[i].length == 1) {
		    if (cc.dots[i] > 0) {
			hpow += cc.dots[i] + cc.genus[i] - 1;
			dots[cc.boundaryComponents[i][0]] = 1;
		    } else if (cc.genus[i] % 2 == 0)
			hpow += cc.genus[i];
		    else
			moreWork[nmoreWork++] = i;
		} else {
		    if (cc.dots[i] > 0) {
			// the dot and the -h terms cancel since dot == h
			for (int j = 0; j<cc.boundaryComponents[i].length; j++)
			    dots[cc.boundaryComponents[i][j]] = 1;
			hpow += cc.dots[i] + cc.genus[i] - 1;
		    } else
			moreWork[nmoreWork++] = i;
		}
	    if (kill)
		continue;
	    int nCdots[][] = new int[1][];
	    int nChpow[] = new int[1];
	    BaseRing nCnum[] = new BaseRing[1];
	    nCdots[0] = dots;
	    nChpow[0] = hpow;
	    nCnum[0] = num;
	    for (int i = 0; i < nmoreWork; i++) {
		int concomp = moreWork[i];
		int nbc = cc.boundaryComponents[concomp].length;
		assert cc.dots[concomp] == 0;
		int newdots[][] = new int[nCdots.length << nbc][cc.nbc];
		int newhpow[] = new int[nChpow.length << nbc];
		BaseRing newnum[] = new BaseRing[nCnum.length << nbc];
		for (int j = 0; j < nCdots.length; j++) {
		    for (int k = 0; k < (1 << nbc); k++) {
			int idx = (j << nbc) + k;
			System.arraycopy(nCdots[j], 0,newdots[idx], 0, cc.nbc);
			newhpow[idx] = nChpow[j];
			newnum[idx] = nCnum[j];
			int nzeros = 0;
			for (int l = 0; l < nbc; l++)
			    if ((k & (1 << l)) == 0) {
				newdots[idx][cc.boundaryComponents[concomp][l]] = 0;
				nzeros++;
			    } else
				newdots[idx][cc.boundaryComponents[concomp][l]] = 1;
			BaseRing nmul = BaseRing.fromInt(0);
			int hmod = 0;
			boolean hset = false;
			for (int l = 0; l < (1 << nzeros); l++) {
			    int ndots = 0;
			    for (int m = 0; m < nzeros; m++)
				if ((l & (1 << m)) == 0)
				    ndots++;
			    if (ndots > 0) {
				if (hset) {
				    if (hmod != nzeros + cc.genus[concomp] - 1)
					throw new AssertionError();
				} else {
				    hmod = nzeros + cc.genus[concomp] - 1;
				    hset = true;
				}
				int n = 1;
				for (int o = 0; o < nzeros - ndots; o++)
				    n = -n;
				nmul = nmul.add(BaseRing.fromInt(n));
			    } else if (cc.genus[concomp] % 2 == 0)
				continue; // coefficient of zero
			    else {
				if (hset) {
				    if (hmod != nzeros + cc.genus[concomp] - 1)
					throw new AssertionError();
				} else {
				    hmod = nzeros + cc.genus[concomp] - 1;
				    hset = true;
				}
				int n = 2;
				for (int o = 0; o < nzeros; o++)
				    n = -n;
				nmul = nmul.add(BaseRing.fromInt(n));
			    }
			}
			newhpow[idx] += hmod;
			newnum[idx] = newnum[idx].multiply(nmul);
		    }
		}
		nCdots = newdots;
		nChpow = newhpow;
		nCnum = newnum;
	    }
	    for (int i = 0; i < nCdots.length; i++) {
		CannedCobordism newcc = new CannedCobordism(top, bottom);
		newcc.connectedComponent = CannedCobordism.counting[newcc.nbc];
		newcc.ncc = newcc.nbc;
		newcc.genus = CannedCobordism.zeros[cc.nbc];
		newcc.dots = nCdots[i];
		newcc.hpower = nChpow[i];
		ret.add(newcc, nCnum[i]);
	    }
	}
	if (ret.n == 0)
	    return null;
	else
	    return ret;
    }

    public LCCC finalizeH() {
	if (n == 0)
	    return null;
	assert top.n == 2 && top.ncycles == 0
	    && bottom.n == 2 && bottom.ncycles == 0;
	LCCC ret = new LCCC(top, bottom);
	CannedCobordism cc = CannedCobordism.isomorphism(top);
	boolean hset = false;
	for (int i = 0; i < n; i++)
	    if (!coefficients[i].isZero()) {
		if (!hset)
		    cc.hpower = cobordisms[i].hpower + cobordisms[i].dots[0]
			+ cobordisms[i].genus[0];
		else if (cc.hpower != cobordisms[i].hpower
			 + cobordisms[i].dots[0] + cobordisms[i].genus[0])
		    throw new AssertionError();
		ret.add(cc, coefficients[i]);
	    }
	if (ret.n == 0)
	    return null;
	else
	    return ret;
    }
}
