package org.katlas.JavaKh;
// CannedCobordisms should be treated as immutable
// don't touch them except just after calling the constructor
public class CannedCobordism implements Comparable {
    public static final int zerosize = 500;
    public static final int zeros[][] = fillZeros();
    public static final int counting[][] = fillCounting();
    private static int[][] fillZeros() {
	int ret[][] = new int[zerosize][];
	for (int i = 0; i < zerosize; i++)
	    ret[i] = new int[i];
	return ret;
    }
    private static int[][] fillCounting() {
	int ret[][] = fillZeros();
	for (int i = 0; i < zerosize; i++)
	    for (int j = 0; j < i; j++)
		ret[i][j] = j;
	return ret;
    }

    public int n;
    public int hpower;
    public Cap top, bottom;
    public int nbc; // number of boundary components
    public int offtop, offbot; // offsets for numbering cycles in top, bottom
    public int component[]; // which boundary component each edge belongs to
    public int ncc; // number of connected components
    public int connectedComponent[]; // which connected component each boundary component belongs to
    public int dots[]; // how many dots each connected component has
    public int genus[]; // the genus of each connected component

    public int boundaryComponents[][]; // which boundary components are connected to each connected component
    public int edges[][]; // which edges are part of each mixed boundary component

    static java.util.Map vcache = new java.util.TreeMap();
    static java.util.Map hcache = new java.util.TreeMap();

    public static void main(String args[]) {
	int n = 8;
	if (checkAssociativity(n))
	//if (checkHorizontal())
	//if (findGenusExample(n))
	    System.out.println("Associativity checks OK");
	else
	    System.out.println("Error in associativity check");
    }

    private CannedCobordism() {
    }

    public CannedCobordism(CannedCobordism cc) {
	// copies most of cc
	n = cc.n;
	top = cc.top;
	bottom = cc.bottom;
	offtop = cc.offtop;
	offbot = cc.offbot;
	component = cc.component;
	connectedComponent = cc.connectedComponent;
	nbc = cc.nbc;
	ncc = cc.ncc;
	// leaves dots and genus null
	dots = genus = null;
	/*dots = new int[ncc];
	  genus = new int[ncc];*/
    }

    public CannedCobordism(Cap t, Cap b) {
	top = t;
	bottom = b;
	n = t.n; // assume b.n == t.n

	component = new int[n];
	for (int i = 0; i < n; i++)
	    component[i] = -1;
	nbc = 0;
	for (int i = 0; i < n; i++)
	    if (component[i] == -1) {
		int j = i;
		do {
		    component[j] = nbc;
		    j = top.pairings[j];
		    component[j] = nbc;
		    j = bottom.pairings[j];
		} while (j != i);
		nbc++;
	    }
	offtop = nbc;
	nbc += top.ncycles;
	offbot = nbc;
	nbc += bottom.ncycles;

	connectedComponent = new int[nbc];

	// let ncc and connectedComponent be set elsewhere
	// let dots and genus be created elsewhere
	// hpower = 0
    }

    public void reverseMaps() { // fills the boundaryComponents array
	if (boundaryComponents != null) // already done
	    return; 
	int numBC[] = new int[ncc];
	for (int i = 0; i < nbc; i++)
	    numBC[connectedComponent[i]]++;
	boundaryComponents = new int[ncc][];
	for (int i = 0; i < ncc; i++)
	    boundaryComponents[i] = new int[numBC[i]];
	int j[] = new int[ncc];
	for (int i = 0; i < nbc; i++) {
	    int k = connectedComponent[i];
	    boundaryComponents[k][j[k]++] = i;
	}
	int nedges[] = new int[offtop];
	for (int i = 0; i < n; i++)
	    nedges[component[i]]++;
	edges = new int[offtop][];
	for (int i = 0; i < offtop; i++)
	    edges[i] = new int[nedges[i]];
	j = new int[offtop];
	for (int i = 0; i < n; i++) {
	    int k = component[i];
	    edges[k][j[k]++] = i;
	}
    }

    public boolean equals(Object o) {
	if (!(o instanceof CannedCobordism))
	    return false;
	CannedCobordism cc = (CannedCobordism) o;
	if (ncc != cc.ncc)
	    return false;
	if (!java.util.Arrays.equals(dots, cc.dots))
	    return false;
	if (!java.util.Arrays.equals(genus, cc.genus))
	    return false;
	if (nbc != cc.nbc)
	    return false;
	if (!java.util.Arrays.equals(connectedComponent,cc.connectedComponent))
	    return false; // assumes the ordering is consistent
	if (n != cc.n)
	    return false;
	if (hpower != cc.hpower)
	    return false;
	if (!java.util.Arrays.equals(component, cc.component))
	    return false;
	if (!top.equals(cc.top))
	    return false;
	if (!bottom.equals(cc.bottom))
	    return false;
	return true;
    }

    public int hashCode() {
	int r = n;
	for (int i = 0; i < n; i++) {
	    r += top.pairings[i] << (i % 16);
	    r += bottom.pairings[i] << (i % 16);
	}
	r += top.ncycles;
	r += bottom.ncycles << 16;
	r += nbc << 4;
	r += ncc << 20;
	for (int i = 0; i < nbc; i++)
	    r += connectedComponent[i] << (i % 16);
	for (int i = 0; i < ncc; i++) {
	    r += dots[i] << (i % 16);
	    r += genus[i] << ((i + 9) % 16);
	}
	r -= hpower;
	return r;
    }

    public int compareTo(Object o) {
	CannedCobordism cc = (CannedCobordism) o;
	if (nbc != cc.nbc)
	    return nbc - cc.nbc;
	for (int i = 0; i < nbc; i++)
	    if (connectedComponent[i] != cc.connectedComponent[i])
		return connectedComponent[i] - cc.connectedComponent[i];
	if (ncc != cc.ncc)
	    return ncc - cc.ncc;
	for (int i = 0; i < ncc; i++) {
	    if (dots[i] != cc.dots[i])
		return dots[i] - cc.dots[i];
	    if (genus[i] != cc.genus[i])
		return genus[i] - cc.genus[i];
	}
	if (n != cc.n)
	    return n - cc.n;
	if (hpower != cc.hpower)
	    return hpower - cc.hpower;
	for (int i = 0; i < n; i++) {
	    if (top.pairings[i] != cc.top.pairings[i])
		return top.pairings[i] - cc.top.pairings[i];
	    if (bottom.pairings[i] != cc.bottom.pairings[i])
		return bottom.pairings[i] - cc.bottom.pairings[i];
	}
	if (top.ncycles != cc.top.ncycles)
	    return top.ncycles - cc.top.ncycles;
	if (bottom.ncycles != cc.bottom.ncycles)
	    return bottom.ncycles - cc.bottom.ncycles;
	return 0;
    }

    public boolean isIsomorphism() { // only works on delooped CCs for now
	if (!top.equals(bottom))
	    return false;
	if (top.ncycles != 0)
	    throw new IllegalArgumentException("isIsomorphism: not delooped");
	if (nbc != ncc)
	    return false;
	if (hpower != 0)
	    return false;
	for (int i = 0; i < nbc; i++) {
	    if (connectedComponent[i] != i)
		return false;
	    if (dots[i] != 0)
		return false;
	    if (genus[i] != 0)
		return false;
	}
	return true;
    }

    public static CannedCobordism isomorphism(Cap c) {
	if (c.ncycles != 0)
	    throw new IllegalArgumentException("Cycles in cap not supported by CannedCobordism.isomorphism()");
	CannedCobordism ret = new CannedCobordism(c, c);
	ret.ncc = ret.nbc;
	ret.connectedComponent = counting[ret.nbc];
	ret.genus = ret.dots = zeros[ret.ncc];
	return ret;
    }

    public static CannedCobordism generateDisconnected(Cap t, Cap b) {
	CannedCobordism ret = new CannedCobordism(t, b);
	ret.ncc = ret.nbc;
	for (int i = 0; i < ret.nbc; i++)
	    ret.connectedComponent[i] = i;
	ret.dots = new int[ret.ncc];
	for (int i = 0; i < ret.ncc; i++)
	    ret.dots[i] = (int) (Math.random() * 10);
	//java.util.Arrays.fill(ret.dots, 2);
	ret.genus = new int[ret.ncc];
	for (int i = 0; i < ret.ncc; i++)
	    ret.genus[i] = 0;//(int) (Math.random() * 5);
	return ret;
    }

    public static CannedCobordism generateConnected(Cap t, Cap b) {
	CannedCobordism ret = new CannedCobordism(t, b);
	ret.ncc = 1;
	java.util.Arrays.fill(ret.connectedComponent, 0);
	ret.dots = new int[ret.ncc];
	ret.dots[0] = (int) (Math.random() * 30);
	ret.genus = new int[ret.ncc];
	ret.genus[0] = (int) (Math.random() * 10);
	return ret;
    }

    public static CannedCobordism generateRandom(Cap t, Cap b) {
	CannedCobordism ret = new CannedCobordism(t, b);
	ret.ncc = 1 + (int) (Math.random() * (ret.nbc - 1));
	for (int i = 0; i < ret.nbc; i++)
	    ret.connectedComponent[i] = (int) (Math.random() * ret.ncc);
	// need to fix the proper consistent ordering
	int j = 0;
	for (int i = 0; i < ret.nbc; i++)
	    if (ret.connectedComponent[i] > j) { // swap values
		int k = ret.connectedComponent[i];
		for (int l = i; l < ret.nbc; l++) {
		    if (ret.connectedComponent[l] == k)
			ret.connectedComponent[l] = j;
		    else if (ret.connectedComponent[l] == j)
			ret.connectedComponent[l] = k;
		}
		j++;
	    } else if (ret.connectedComponent[i] == j)
		j++;
	ret.dots = new int[ret.ncc];
	for (int i = 0; i < ret.ncc; i++)
	    ret.dots[i] = (int) (Math.random() * 10);
	//java.util.Arrays.fill(ret.dots, 2);
	ret.genus = new int[ret.ncc];
	//for (int i = 0; i < ret.ncc; i++)
	//ret.genus[i] = (int) (Math.random() * 5);
	System.arraycopy(ret.dots, 0, ret.genus, 0, ret.ncc);
	return ret;
    }

    public static boolean findGenusExample(int n) {
	Cap[] caps = Cap.generate(n);
	for (int i = 0; i < caps.length; i++)
	    for (int j = 0; j < caps.length; j++)
		for (int k = 0; k < caps.length; k++) {
		    CannedCobordism a = generateDisconnected(caps[i], caps[j]),
			b = generateDisconnected(caps[j], caps[k]);
		    CannedCobordism c = b.compose(a);
		    for (int l = 0; l < c.ncc; l++)
			if (c.genus[l] > 0) {
			    int nbc = 0;
			    for (int m = 0; m < c.nbc; m++)
				if (c.connectedComponent[m] == l)
				    nbc++;
			    if (nbc == 1)
				return true;
			}
		}
	return false;
    }

    public static boolean checkAssociativity(int n) {
	Cap[] caps = Cap.generate(n);
	for (int i = 0; i < caps.length; i++)
	    for (int j = 0; j < caps.length; j++)
		for (int k = 0; k < caps.length; k++)
		    for (int l = 0; l < caps.length; l++) {
			CannedCobordism
			    a = generateRandom(caps[i], caps[j]),
			    b = generateRandom(caps[j], caps[k]),
			    c = generateRandom(caps[k], caps[l]);
			CannedCobordism d = (c.compose(b)).compose(a);
			CannedCobordism e = c.compose(b.compose(a));
			if (!(d.equals(e)))
			    return false;
			int ndots = 0; // check that dots add up properly
			for (int m = 0; m < a.ncc; m++)
			    ndots += a.dots[m];
			for (int m = 0; m < b.ncc; m++)
			    ndots += b.dots[m];
			for (int m = 0; m < c.ncc; m++)
			    ndots += c.dots[m];
			for (int m = 0; m < d.ncc; m++)
			    ndots -= d.dots[m];
			if (ndots != 0)
			    return false;
			if (!java.util.Arrays.equals(d.dots, d.genus))
			    i = i + 0;
		    }
	return true;
    }

    public static boolean checkHorizontal() {
	Cap caps[] = Cap.generate(6);
	for (int i = 0; i < caps.length; i++)
	    for (int j = 0; j < caps.length; j++)
		for (int k = 0; k < caps.length; k++)
		    for (int l = 0; l < caps.length; l++) {
			// there is no reason for the caps to match up
			// like this, but all of the combinations is
			// a very big number
			CannedCobordism
			    a = generateRandom(caps[i], caps[j]),
			    b = generateRandom(caps[j], caps[k]),
			    c = generateRandom(caps[k], caps[l]);
			CannedCobordism d = a.compose(1, b, 4, 2).compose(5, c, 3, 3);
			CannedCobordism e = a.compose(1, b.compose(1, c, 3, 3), 0, 2);
			if (!d.equals(e))
			    return false;
			int ndots = 0; // check that dots add up properly
			for (int m = 0; m < a.ncc; m++)
			    ndots += a.dots[m];
			for (int m = 0; m < b.ncc; m++)
			    ndots += b.dots[m];
			for (int m = 0; m < c.ncc; m++)
			    ndots += c.dots[m];
			for (int m = 0; m < d.ncc; m++)
			    ndots -= d.dots[m];
			if (ndots != 0)
			    return false;
			if (!java.util.Arrays.equals(d.dots, d.genus))
			    i = i + 0;
		    }
	return true;
    }

    // sanity check
    public boolean check() {
	if (nbc > 0 && ncc < 1)
	    return false;
	if (ncc < 0)
	    return false;
	for (int i = 0; i < nbc; i++)
	    if (connectedComponent[i] < 0 || connectedComponent[i] >= ncc)
		return false;
	return true;
    }
    
    public CannedCobordism compose(CannedCobordism cc) {// cc on top of this
	assert top.equals(cc.bottom);
	if (ncc + cc.ncc > 20)
	    return compose2(cc);
	VComposeInput ci = new VComposeInput(this, cc);
	ComposeOutput co = (ComposeOutput) vcache.get(ci);
	if (co == null) {
	    co = new ComposeOutput(this, cc);
	    vcache.put(ci, co);
	}
	return co.get(this, cc);
    }

    // new vertical composition
    public CannedCobordism compose2(CannedCobordism cc) {//cc on top of this
	assert top.equals(cc.bottom);

	reverseMaps();
	cc.reverseMaps();
	CannedCobordism ret = new CannedCobordism(cc.top, bottom);
	ret.hpower = hpower + cc.hpower;

	for (int i = 0; i < ret.nbc; i++)
	    ret.connectedComponent[i] = i;
	int midConComp[] = new int[top.ncycles];
	for (int i = 0; i < top.ncycles; i++)
	    midConComp[i] = -2 - i;
	int rdots[] = new int[ncc + cc.ncc + ret.nbc];
	int mdots[] = new int[ncc + cc.ncc + ret.nbc + 2];
	int udots[] = new int[ncc + cc.ncc + ret.nbc];
	int ugenus[] = new int[ncc + cc.ncc + ret.nbc];
	int unconnected = 0;
	// add in connected components from this and cc, one by one
	for (int i = 0; i < ncc; i++) {
	    int reti = -1;
	    // pick a reti
	    for (int j = 0; j < boundaryComponents[i].length; j++) {
		int k = boundaryComponents[i][j];
		if (k < offtop) {
		    for (int l = 0; l < edges[k].length; l++) {
			reti = ret.connectedComponent[ret.component[edges[k][l]]];
			if (reti >= 0)
			    break;
		    }
		} else if (k < offbot) // middle cycle
		    reti = midConComp[k - offtop];
		else // bottom cycle
		    reti = ret.connectedComponent[k - offbot + ret.offbot];
		if (reti >= 0)
		    break;
	    }

	    if (reti == -1) {
		ugenus[unconnected] = genus[i];
		udots[unconnected++] = dots[i];
		continue;
	    } else if (reti >= 0)
		rdots[reti] += dots[i];
	    else
		mdots[-2 - reti] += dots[i];

	    for (int j = 0; j < boundaryComponents[i].length; j++) {
		if (boundaryComponents[i][j] < offtop) { //mixed
		    for (int k=0;k<edges[boundaryComponents[i][j]].length;k++){
			int rettest = ret.connectedComponent[ret.component[edges[boundaryComponents[i][j]][k]]];
			if (rettest != reti) {
			    for (int l = 0; l < ret.nbc; l++)
				if (ret.connectedComponent[l] == rettest)
				    ret.connectedComponent[l] = reti;
			    for (int l = 0; l < midConComp.length; l++)
				if (midConComp[l] == rettest)
				    midConComp[l] = reti;
			    rdots[reti] += rdots[rettest];
			}
		    }
		} else if (boundaryComponents[i][j] < offbot) { //middle
		    // doesn't match previous implementation
		    // both are probably correct
		    int mtest = midConComp[boundaryComponents[i][j] - offtop];
		    if (mtest != reti) {
			if (mtest >= 0)
			    for (int k = 0; k < ret.nbc; k++)
				if (ret.connectedComponent[k] == mtest)
				    ret.connectedComponent[k] = reti;
			for (int k = 0; k < midConComp.length; k++)
			    if (midConComp[k] == mtest)
				midConComp[k] = reti;
			if (reti >= 0)
			    if (mtest >= 0)
				rdots[reti] += rdots[mtest];
			    else
				rdots[reti] += mdots[-2 - mtest];
			else
			    if (mtest >= 0)
				mdots[-2 - reti] += rdots[mtest];
			    else
				mdots[-2 - reti] += mdots[-2 - mtest];
		    }
		} else { // bottom
		    int rtest = ret.connectedComponent[boundaryComponents[i][j] - offbot + ret.offbot];
		    if (rtest != reti) {
			for (int k = 0; k < ret.nbc; k++)
			    if (ret.connectedComponent[k] == rtest)
				ret.connectedComponent[k] = reti;
			for (int k = 0; k < midConComp.length; k++)
			    if (midConComp[k] == rtest)
				midConComp[k] = reti;
			rdots[reti] += rdots[rtest];
		    }
		}
	    }
	}
	// same with cc
	for (int i = 0; i < cc.ncc; i++) {
	    int reti = -1;
	    for (int j = 0; j < cc.boundaryComponents[i].length; j++) {
		int k = cc.boundaryComponents[i][j];
		if (k < cc.offtop) {
		    for (int l = 0; l < cc.edges[k].length; l++) {
			reti = ret.connectedComponent[ret.component[cc.edges[k][l]]];
			if (reti >= 0)
			    break;
		    }
		} else if (k < cc.offbot) // top cycle
		    reti = ret.connectedComponent[k - cc.offtop + ret.offtop];
		else // middle cycle
		    reti = midConComp[k - cc.offbot];
		if (reti >= 0)
		    break;
	    }

	    if (reti == -1) {
		ugenus[unconnected] = cc.genus[i];
		udots[unconnected++] = cc.dots[i];
		continue;
	    } else if (reti >= 0)
		rdots[reti] += cc.dots[i];
	    else
		mdots[-2 - reti] += cc.dots[i];

	    for (int j = 0; j < cc.boundaryComponents[i].length; j++) {
		if (cc.boundaryComponents[i][j] < cc.offtop) { //mixed
		    for (int k = 0; k < cc.edges[cc.boundaryComponents[i][j]].length; k++) {
			int rtest = ret.connectedComponent[ret.component[cc.edges[cc.boundaryComponents[i][j]][k]]];
			if (rtest != reti) {
			    for (int l = 0; l < ret.nbc; l++)
				if (ret.connectedComponent[l] == rtest)
				    ret.connectedComponent[l] = reti;
			    for (int l = 0; l < midConComp.length; l++)
				if (midConComp[l] == rtest)
				    midConComp[l] = reti;
			    rdots[reti] += rdots[rtest];
			}
		    }
		} else if (cc.boundaryComponents[i][j] < cc.offbot) { // top
		    int rtest = ret.connectedComponent[cc.boundaryComponents[i][j] - cc.offtop + ret.offtop];
		    if (rtest != reti) {
			for (int k = 0; k < ret.nbc; k++)
			    if (ret.connectedComponent[k] == rtest)
				ret.connectedComponent[k] = reti;
			for (int k = 0; k < midConComp.length; k++)
			    if (midConComp[k] == rtest)
				midConComp[k] = reti;
			rdots[reti] += rdots[rtest];
		    }
		} else { // middle
		    int mtest = midConComp[cc.boundaryComponents[i][j] - cc.offbot];
		    if (mtest != reti) {
			if (mtest >= 0)
			    for (int k = 0; k < ret.nbc; k++)
				if (ret.connectedComponent[k] == mtest)
				    ret.connectedComponent[k] = reti;
			for (int k = 0; k < midConComp.length; k++)
			    if (midConComp[k] == mtest)
				midConComp[k] = reti;
			if (reti >= 0)
			    if (mtest >= 0)
				rdots[reti] += rdots[mtest];
			    else
				rdots[reti] += mdots[-2 - mtest];
			else
			    if (mtest >= 0)
				mdots[-2 - reti] += rdots[mtest];
			    else
				mdots[-2 - reti] += mdots[-2 - mtest];
		    }
		}
	    }
	}

	// now, relabel ret's connected components in ascending order
	ret.ncc = 0;
	for (int i = 0; i < ret.nbc; i++) {
	    if (ret.connectedComponent[i] > ret.ncc) { // swap the values
		int j = ret.connectedComponent[i];
		for (int k = i; k < ret.nbc; k++) {
		    if (ret.connectedComponent[k] == j)
			ret.connectedComponent[k] = ret.ncc;
		    else if (ret.connectedComponent[k] == ret.ncc)
			ret.connectedComponent[k] = j;
		}
		for (int k = 0; k < midConComp.length; k++) { // used for genus
		    if (midConComp[k] == j)
			midConComp[k] = ret.ncc;
		    else if (midConComp[k] == ret.ncc)
			midConComp[k] = j;
		}
		int tmp = rdots[ret.ncc];
		rdots[ret.ncc] = rdots[j];
		rdots[j] = tmp;
		ret.ncc++;
	    } else if (ret.connectedComponent[i] == ret.ncc) // already set up
		ret.ncc++;
	}

	// genus
	ret.reverseMaps(); // since ret.ncc may change later, this may need to be undone later!
	int rgenus[] = new int[ret.ncc]; 
	for (int i = 0; i < ret.ncc; i++) {
	    int b = ret.boundaryComponents[i].length;
	    int x = 0; // Euler characteristic
	    // add genus and boundary components from this
	    for (int j = 0; j < ncc; j++) {
		for (int k = 0; k < boundaryComponents[j].length; k++) {
		    boolean found = false;
		    if (boundaryComponents[j][k] < offtop) { // mixed
			for (int l = 0; l < edges[boundaryComponents[j][k]].length; l++)
			    if (ret.connectedComponent[ret.component[edges[boundaryComponents[j][k]][l]]] == i) {
				found = true;
				break;
			    }
		    } else if (boundaryComponents[j][k] < offbot) { // middle
			if (midConComp[boundaryComponents[j][k]- offtop] == i)
			    found = true;
		    } else { // bottom
			if (ret.connectedComponent[boundaryComponents[j][k] - offbot + ret.offbot] == i)
			    found = true;
		    }
		    if (found) {
			x += 2 - 2 * genus[j] - boundaryComponents[j].length;
			// this is a convenient place to add in the face
			// and two edges (= -1) for the joins
			// conveniently, the adjustment is zero for
			// joining middle cycles
			// this is to be done only here
			// and not for cc too
			int njoins = 0;
			for (int l = 0; l < boundaryComponents[j].length; l++)
			    if (boundaryComponents[j][l] < offtop)
				njoins += edges[boundaryComponents[j][l]].length;
			    else
				break; // uses fact that bC is in order
			// njoins should be even
			if (njoins % 2 != 0)
			    throw new AssertionError();
			x -= njoins / 2;
			break;
		    }		
		}
	    }
	    // same for cc
	    for (int j = 0; j < cc.ncc; j++) {
		for (int k = 0; k < cc.boundaryComponents[j].length; k++) {
		    boolean found = false;
		    if (cc.boundaryComponents[j][k] < cc.offtop) { // mixed
			for (int l = 0; l < cc.edges[cc.boundaryComponents[j][k]].length; l++)
			    if (ret.connectedComponent[ret.component[cc.edges[cc.boundaryComponents[j][k]][l]]] == i) {
				found = true;
				break;
			    }
		    } else if (cc.boundaryComponents[j][k] < cc.offbot) { //top
			if (ret.connectedComponent[cc.boundaryComponents[j][k] - cc.offtop + ret.offtop] == i)
			    found = true;
		    } else { // middle
			if (midConComp[cc.boundaryComponents[j][k] - cc.offbot] == i)
			    found = true;
		    }
		    if (found) {
			x += 2 - 2 * cc.genus[j] - cc.boundaryComponents[j].length;
			break;
		    }
		}
	    }
	    int g = 2 - b - x; // twice the genus
	    if (g % 2 != 0 || g < 0)
		throw new AssertionError();
	    rgenus[i] = g / 2;
	}

	// same as old way
	for (int i = 0; i < midConComp.length; i++) {
	    boolean found = false;
	    int g = 1;
	    for (int j = 0; j < midConComp.length; j++) {
		if (midConComp[j] == -2 - i) {
		    found = true;
		    // each connection should add 1 to the genus
		    g++;
		}
	    }
	    if (found) {
		for (int j = 0; j < ncc; j++)
		    for (int k = offtop; k < offbot; k++)
			if (midConComp[k - offtop] == -2 - i)
			    if (connectedComponent[k] == j) {
				// each extra component should subtract 1
				// from the genus
				g += genus[j] - 1;
				break;
			    }
		for (int j = 0; j < cc.ncc; j++)
		    for (int k = cc.offbot; k < cc.nbc; k++)
			if (midConComp[k - cc.offbot] == -2 - i)
			    if (cc.connectedComponent[k] == j) {
				g += cc.genus[j] - 1;
				break;
			    }
		ugenus[unconnected] = g;
		udots[unconnected++] = mdots[i];
	    }
	}

	// resort the unconnected: first by genus, then dots
	// this is a bit lazy...
	// should be unnecessary
	/*long uncon[] = new long[unconnected];
	for (int i = 0; i < unconnected; i++)
	    uncon[i] = (((long) ugenus[i]) << 32) | udots[i];
	java.util.Arrays.sort(uncon);
	for (int i = 0; i < unconnected; i++) {
	    ugenus[i] = (int) (uncon[i] >>> 32);
	    udots[i] = (int) (uncon[i] & 0xffffffff);
	    }*/

	int rncc = ret.ncc;
	ret.ncc += unconnected;
	ret.dots = new int[ret.ncc];
	System.arraycopy(rdots, 0, ret.dots, 0, rncc);
	System.arraycopy(udots, 0, ret.dots, rncc, unconnected);
	ret.genus = new int[ret.ncc];
	System.arraycopy(rgenus, 0, ret.genus, 0, rncc);
	System.arraycopy(ugenus, 0, ret.genus, rncc, unconnected);
	// perhaps the unconnected should be sorted differently?

	// undo ret.reverseMaps if necessary
	if (unconnected > 0) {
	    ret.boundaryComponents = null;
	    ret.edges = null;
	}
	// and we are done (hopefully)
	assert ret.check();
	return ret;
    }
    
    public CannedCobordism compose(int start, CannedCobordism cc, int cstart,
				   int nc) {
	if (ncc + cc.ncc > 20)
	    return compose2(start, cc, cstart, nc);
	HComposeInput ci = new HComposeInput(this, start, cc, cstart, nc);
	ComposeOutput co = (ComposeOutput) hcache.get(ci);
	if (co == null) {
	    co = new ComposeOutput(this, start, cc, cstart, nc);
	    hcache.put(ci, co);
	}
	return co.get(this, cc);
    }

    // horizontal composition
    public CannedCobordism compose2(int start, CannedCobordism cc, int cstart,
				    int nc) {
	assert check() && cc.check();
	/*if (!check() || !cc.check())
	  throw new AssertionError();*/
	int tjoins[] = new int[nc], bjoins[] = new int[nc];
	Cap rtop = top.compose(start, cc.top, cstart, nc, tjoins);
	Cap rbot = bottom.compose(start, cc.bottom, cstart, nc, bjoins);
	CannedCobordism ret = new CannedCobordism(rtop, rbot);
	ret.hpower = hpower + cc.hpower;

	for (int i = 0; i < ret.nbc; i++)
	    ret.connectedComponent[i] = i;
	int midConComp[] = new int[nc];
	for (int i = 0; i < nc; i++)
	    midConComp[i] = -2 - i;
	int rdots[] = new int[ret.nbc + nc + 2];
	int mdots[] = new int[nc];
	int udots[] = new int[ncc + cc.ncc];
	int ugenus[] = new int[ncc + cc.ncc];
	int unconnected = 0;
	// add in connectedComponents from this and cc, one by one
	for (int i = 0; i < ncc; i++) {

	    int reti = -1;
	    // pick a good reti now
	    for (int j = offtop; j < nbc; j++)
		if (connectedComponent[j] == i) {
		    int retj;
		    if (j < offbot)
			retj = j - offtop + ret.offtop;
		    else
			retj = j - offbot + ret.offbot;
		    reti = ret.connectedComponent[retj];
		    break;
		}
	    if (reti == -1)
		for (int j = 0; j < n; j++) { // here, j is ret's j
		    int thisj = (j + start + nc) % n;
		    if (connectedComponent[component[thisj]] == i) {
			if (j < n - nc)
			    reti = ret.connectedComponent[ret.component[j]];
			else
			    // these are the last choice
			    reti = midConComp[j - n + nc];
			if (reti >= 0)
			    break;
		    }
		}
	    if (reti >= 0)
		rdots[reti] += dots[i];
	    else if (reti < -1)
		mdots[-2 - reti] += dots[i];

	    if (reti != -1)
		for (int j = 0; j < offtop; j++) { // mixed cycles
		    if (connectedComponent[j] == i) {
			for (int k = 0; k < n; k++)
			    if (component[k] == j) {
				int retk = (k - start - nc + 2 * n) % n;
				int tmp;
				if (retk < n - nc)
				    tmp = ret.connectedComponent[ret.component[retk]];
				else
				    tmp = midConComp[retk - n + nc];
				if (tmp != reti) {
				    if (tmp >= 0)
					for (int l = 0; l < ret.nbc; l++)
					    if (ret.connectedComponent[l]==tmp)
						ret.connectedComponent[l]=reti;
				    for (int l = 0; l < nc; l++)
					if (midConComp[l] == tmp)
					    midConComp[l] = reti;
				    if (tmp >= 0)
					if (reti >= 0)
					    rdots[reti] += rdots[tmp];
					else
					    mdots[-2 - reti] += rdots[tmp];
				    else
					if (reti >= 0)
					    rdots[reti] += mdots[-2 - tmp];
					else
					    mdots[-2 - reti] += mdots[-2 -tmp];
				}
			    }
		    }
		}
	    if (reti >= 0) // otherwise there is nothing to do here
		for (int j = offtop; j < nbc; j++) { // top/bottom cycles
		    if (connectedComponent[j] == i) {
			int retj;
			if (j < offbot)
			    retj = j - offtop + ret.offtop;
			else
			    retj = j - offbot + ret.offbot;
			if (ret.connectedComponent[retj] != reti) {
			    int tmp = ret.connectedComponent[retj];
			    for (int k = 0; k < ret.nbc; k++)
				if (ret.connectedComponent[k] == tmp)
				    ret.connectedComponent[k] = reti;
			    for (int k = 0; k < nc; k++)
				if (midConComp[k] == tmp)
				    midConComp[k] = reti;
			    rdots[reti] += rdots[tmp];
			}
		    }
		}
	    if (reti == -1) {
		ugenus[unconnected] = genus[i];
		udots[unconnected++] = dots[i];
	    }
	}
	for (int i = 0; i < cc.ncc; i++) {
	    int reti = -1;
	    // pick a good reti now
	    for (int j = cc.offtop; j < cc.nbc; j++)
		if (cc.connectedComponent[j] == i) {
		    int retj;
		    if (j < cc.offbot)
			retj = j - cc.offtop + offbot - offtop + ret.offtop;
		    else
			retj = j - cc.offbot + nbc - offbot + ret.offbot;
		    reti = ret.connectedComponent[retj];
		    break;
		}
	    if (reti == -1)
		for (int j = 0; j < cc.n; j++) { // here, j is ret's j
		    int thisj = (j + cstart + nc) % cc.n;
		    if (cc.connectedComponent[cc.component[thisj]] == i) {
			if (j < cc.n - nc)
			    reti=ret.connectedComponent[ret.component[j+n-nc]];
			else
			    reti = midConComp[cc.n - j - 1];
			if (reti >= 0)
			    break;
		    }
		}
	    if (reti >= 0)
		rdots[reti] += cc.dots[i];
	    else if (reti < -1)
		mdots[-2 - reti] += cc.dots[i];
	    
	    if (reti != -1)
		for (int j = 0; j < cc.offtop; j++) {
		    if (cc.connectedComponent[j] == i) {
			for (int k = 0; k < cc.n; k++)
			    if (cc.component[k] == j) {
				int retk = (k - cstart - nc + 2 * cc.n) % cc.n;
				int tmp;
				if (retk < cc.n - nc)
				    tmp = ret.connectedComponent[ret.component[retk + n - nc]];
				else
				    tmp = midConComp[cc.n - retk - 1];
				if (tmp != reti) {
				    if (tmp >= 0)
					for (int l = 0; l < ret.nbc; l++)
					    if (ret.connectedComponent[l]==tmp)
						ret.connectedComponent[l]=reti;
				    for (int l = 0; l < nc; l++)
					if (midConComp[l] == tmp)
					    midConComp[l] = reti;
				    if (tmp >= 0)
					if (reti >= 0)
					    rdots[reti] += rdots[tmp];
					else
					    mdots[-2 - reti] += rdots[tmp];
				    else
					if (reti >= 0)
					    rdots[reti] += mdots[-2 - tmp];
					else
					    mdots[-2 - reti] += mdots[-2 -tmp];
				}
			    }
		    }
		}
	    if (reti >= 0)
		for (int j = cc.offtop; j < cc.nbc; j++) {
		    if (cc.connectedComponent[j] == i) {
			int retj;
		    //top/bottom cycles from cc will come after those from this
			if (j < cc.offbot)
			    retj =j - cc.offtop + offbot - offtop + ret.offtop;
			else
			    retj = j - cc.offbot + (nbc - offbot) + ret.offbot;
			if (ret.connectedComponent[retj] != reti) {
			    int tmp = ret.connectedComponent[retj];
			    for (int k = 0; k < ret.nbc; k++)
				if (ret.connectedComponent[k] == tmp)
				    ret.connectedComponent[k] = reti;
			    for (int k = 0; k < nc; k++)
				if (midConComp[k] == tmp)
				    midConComp[k] = reti;
			    rdots[reti] += rdots[tmp];
		    }
		}
	    }
	    if (reti == -1) {
		ugenus[unconnected] = cc.genus[i];
		udots[unconnected++] = cc.dots[i];
	    }
	}

	// handle newly created cycles
	for (int i = 0; i < nc; i++) {
	    boolean found = false;
	    for (int j = 0; j < nc; j++) {
		if (tjoins[j] == i) {
		    int reti = ret.offbot - 1 - i;
		    if (midConComp[j] >= 0)
			ret.connectedComponent[reti] = midConComp[j];
		    else {
			ret.connectedComponent[reti] = ret.nbc - midConComp[j];
			rdots[ret.nbc - midConComp[j]] =
			    mdots[-2 - midConComp[j]];
			// want midConComp to have the same labels
			// as ret.connectedComponent in order for
			// genus calculation to work
			midConComp[j] = ret.nbc - midConComp[j];
		    }
		    found = true;
		    break;
		}
	    }
	    if (!found)
		break;
	}
	for (int i = 0; i < nc; i++) {
	    boolean found = false;
	    for (int j = 0; j < nc; j++)
		if (bjoins[j] == i) {
		    int reti = ret.nbc - 1 - i;
		    if (midConComp[j] >= 0)
			ret.connectedComponent[reti] = midConComp[j];
		    else {
			ret.connectedComponent[reti] = ret.nbc - midConComp[j];
			rdots[ret.nbc - midConComp[j]] =
			    mdots[-2 - midConComp[j]];
			midConComp[j] = ret.nbc - midConComp[j];
		    }
		    found = true;
		    break;
		}
	    if (!found)
		break;
	}

	// HERE IS THE BEST PLACE TO INSERT GENUS CALCULATIONS
	// they should be the same as for vertical
	// but rgenus[] needs to be larger as relabelling has not yet occurred
	int rgenus[] = new int[ret.nbc + nc + 2];
	for (int i = 0; i < rgenus.length; i++) {
	    int b = 0; // boundary components in ret
	    for (int j = 0; j < ret.nbc; j++)
		if (ret.connectedComponent[j] == i)
		    b++;
	    if (b == 0)
		continue; // I think this should be here
	    int x = 0; // Euler characteristic
	    // genus + boundary components from this
	    for (int j = 0; j < ncc; j++) {
		for (int k = 0; k < nbc; k++)
		    if (connectedComponent[k] == j) {
			boolean found = false;
			if (k < offtop) {
			    for (int l = 0; l < n; l++)
				if (component[l] == k) {
				    int retl = (l - start - nc + 2 * n) % n;
				    if (retl < n - nc) {
					if (ret.connectedComponent[ret.component[retl]] == i) {
					    found = true;
					    break;
					}
				    } else if (midConComp[retl - n + nc] == i) {
					found = true;
					break;
				    }
				}
			} else if (k < offbot) {
			    if (ret.connectedComponent[k - offtop + ret.offtop]
				== i)
				found = true;
			} else {
			    if (ret.connectedComponent[k - offbot + ret.offbot]
				== i)
				found = true;
			}
			if (found) {
			    x += 2 - 2 * genus[j];
			    for (int l = 0; l < nbc; l++)
				if (connectedComponent[l] == j)
				    x--;
			    // add in the joins
			    // only here, not for cc too
			    int njoins = 0;
			    for (int l = 0; l < nc; l++)
				if (connectedComponent[component[(l + start) % n]] == j)
				    njoins++;
			    // njoins should be even
			    //if (njoins % 2 != 0)
			    //throw new AssertionError();
			    x -= njoins;// / 2;
			    break;
			}
		    }
	    }
	    // the same for cc:
	    for (int j = 0; j < cc.ncc; j++) {
		for (int k = 0; k < cc.nbc; k++)
		    if (cc.connectedComponent[k] == j) {
			boolean found = false;
			if (k < cc.offtop) {
			    for (int l = 0; l < cc.n; l++)
				if (cc.component[l] == k) {
				    int retl = (l - cstart - nc + 2*cc.n)%cc.n;
				    if (retl < cc.n - nc) {
					if (ret.connectedComponent[ret.component[retl + n - nc]] == i) {
					    found = true;
					    break;
					}
				    } else if (midConComp[cc.n - retl - 1] == i) {
					found = true;
					break;
				    }
				}
			} else if (k < cc.offbot) {
			    if (ret.connectedComponent[k - cc.offtop + offbot - offtop + ret.offtop]
				== i)
				found = true;
			} else {
			    if (ret.connectedComponent[k - cc.offbot + nbc - offbot + ret.offbot]
				== i)
				found = true;
			}
			if (found) {
			    x += 2 - 2 * cc.genus[j];
			    for (int l = 0; l < cc.nbc; l++)
				if (cc.connectedComponent[l] == j)
				    x--;
			    break;
			}
		    }
	    }

	    int g = 2 - b - x; // twice the genus
	    if (g % 2 != 0 || g < 0)
		throw new AssertionError();
	    rgenus[i] = g / 2;
	}

	/* elaborate seeming sort of top, bottom cycles
	 * this renumbers the boundary components
	 * for consistency, genus and dots must also be sorting factors
	 * therefore, the sorting priority is:
	 * 1. connections with mixed go first
	 * 2. top connections
	 * 3. bottom connections
	 * 4. genus (add this later)
	 * 5. dots
	 * 6. (arbitrary) connectedComponent number
	 */
	// no longer sorting
	// therefore, this is only consistent after delooping has occurred
	/*int mconnections[] = new int[ret.nbc + nc + 2];
	for (int i = 0; i < ret.offtop; i++)
	    mconnections[ret.connectedComponent[i]]++;
	int tconnections[] = new int[ret.nbc + nc + 2];
	for (int i = ret.offtop; i < ret.offbot; i++)
	    tconnections[ret.connectedComponent[i]]++;
	int bconnections[] = new int[ret.nbc + nc + 2];
	for (int i = ret.offbot; i < ret.nbc; i++)
	    bconnections[ret.connectedComponent[i]]++;
	int sortarr[][] = new int[ret.offbot - ret.offtop][6];
	for (int i = 0; i < sortarr.length; i++) {
	    int concomp = ret.connectedComponent[ret.offtop + i];
	    sortarr[i][0] = mconnections[concomp];
	    sortarr[i][1] = tconnections[concomp];
	    sortarr[i][2] = bconnections[concomp];
	    sortarr[i][3] = rdots[concomp];
	    sortarr[i][4] = rgenus[concomp];
	    sortarr[i][5] = concomp;
	    }*/
	java.util.Comparator comp = new java.util.Comparator() {
		public int compare(Object o1, Object o2) {
		    int a[] = (int[]) o1, b[] = (int[]) o2;
		    for (int i = 0; i < a.length; i++)
			if (a[i] != b[i])
			    return a[i] - b[i];
		    return 0;
		}
	    };
	/*java.util.Arrays.sort(sortarr, comp);
	for (int i = 0; i < sortarr.length; i++)
	    ret.connectedComponent[ret.offtop + i] = sortarr[i][5];
	sortarr = new int[ret.nbc - ret.offbot][6];
	for (int i = 0; i < sortarr.length; i++) {
	    int concomp = ret.connectedComponent[ret.offbot + i];
	    sortarr[i][0] = mconnections[concomp];
	    sortarr[i][1] = tconnections[concomp];
	    sortarr[i][2] = bconnections[concomp];
	    sortarr[i][3] = rdots[concomp];
	    sortarr[i][4] = rgenus[concomp];
	    sortarr[i][5] = concomp;
	}
	java.util.Arrays.sort(sortarr, comp);
	for (int i = 0; i < sortarr.length; i++)
	ret.connectedComponent[ret.offbot + i] = sortarr[i][5];*/
	/*int connections[] = new int[ret.nbc + nc + 2];
	for (int i = 0; i < ret.offtop; i++)
	    connections[ret.connectedComponent[i]] = -1;
	for (int i = ret.offtop; i < ret.nbc; i++)
	    if (connections[ret.connectedComponent[i]] != -1)
		if (i < ret.offbot)
		    connections[ret.connectedComponent[i]] += 1 << 16;
		else
		    connections[ret.connectedComponent[i]]++;
	long sortarr[] = new long[ret.offbot - ret.offtop];
	for (int i = 0; i < sortarr.length; i++)
	    sortarr[i] = ((long) connections[ret.connectedComponent[ret.offtop + i]] << 32) | ret.connectedComponent[ret.offtop + i];
	java.util.Arrays.sort(sortarr);
	for (int i = 0; i < sortarr.length; i++)
	    ret.connectedComponent[ret.offtop + i] = (int) (sortarr[i] & 0xffffffff);
	sortarr = new long[ret.nbc - ret.offbot];
	for (int i = 0; i < sortarr.length; i++)
	    sortarr[i] = ((long) connections[ret.connectedComponent[ret.offbot + i]] << 32) | ret.connectedComponent[ret.offbot + i];
	java.util.Arrays.sort(sortarr);
	for (int i = 0; i < sortarr.length; i++)
	    ret.connectedComponent[ret.offbot + i] = (int) (sortarr[i] & 0xffffffff);
	*/

	// relabel ret's connected components in ascending order
	ret.ncc = 0;
	for (int i = 0; i < ret.nbc; i++) {
	    if (ret.connectedComponent[i] > ret.ncc) {
		int j = ret.connectedComponent[i];
		for (int k = i; k < ret.nbc; k++) {
		    if (ret.connectedComponent[k] == j)
			ret.connectedComponent[k] = ret.ncc;
		    else if (ret.connectedComponent[k] == ret.ncc)
			ret.connectedComponent[k] = j;
		}
		// for genus, do something similar with midConComp?
		int tmp = rdots[ret.ncc];
		rdots[ret.ncc] = rdots[j];
		rdots[j] = tmp;
		tmp = rgenus[ret.ncc];
		rgenus[ret.ncc] = rgenus[j];
		rgenus[j] = tmp;
		ret.ncc++;
	    } else if (ret.connectedComponent[i] == ret.ncc)
		ret.ncc++;
	}

	// more sorting of top, bottom cycles
	// REMOVED: see above
	/*java.util.Arrays.sort(ret.connectedComponent, ret.offtop, ret.offbot);
	  java.util.Arrays.sort(ret.connectedComponent, ret.offbot, ret.nbc);*/

	// resort the unconnected: by genus, dots
	int sortarr[][] = new int[unconnected][2];
	for (int i = 0; i < unconnected; i++) {
	    sortarr[i][0] = ugenus[i];
	    sortarr[i][1] = udots[i];
	}
	java.util.Arrays.sort(sortarr, comp);
	//java.util.Arrays.sort(udots, 0, unconnected);

	int rncc = ret.ncc;
	ret.ncc += unconnected;
	ret.dots = new int[ret.ncc];
	System.arraycopy(rdots, 0, ret.dots, 0, rncc);
	//System.arraycopy(udots, 0, ret.dots, rncc, unconnected);
	for (int i = 0; i < unconnected; i++)
	    ret.dots[i + rncc] = sortarr[i][1];
	ret.genus = new int[ret.ncc];
	System.arraycopy(rgenus, 0, ret.genus, 0, rncc);
	for (int i = 0; i < unconnected; i++)
	    ret.genus[i + rncc] = sortarr[i][0];

	assert ret.check();
	return ret;
    }

    private class VComposeInput implements Comparable {
	int n;
	Cap top, middle, bottom;
	int topnbc, botnbc;
	int topncc, botncc;
	int topConnectedComponent[], botConnectedComponent[];
	public VComposeInput(CannedCobordism a, CannedCobordism b) {
	    assert a.n == b.n;
	    n = a.n;
	    assert a.top.equals(b.bottom);
	    top = b.top;
	    middle = b.bottom;
	    bottom = a.bottom;
	    topnbc = b.nbc;
	    botnbc = a.nbc;
	    topncc = b.ncc;
	    botncc = a.ncc;
	    topConnectedComponent = b.connectedComponent;
	    botConnectedComponent = a.connectedComponent;
	}

	public int compareTo(Object o) {
	    VComposeInput ci = (VComposeInput) o;
	    if (n != ci.n)
		return n - ci.n;
	    if (topnbc != ci.topnbc)
		return topnbc - ci.topnbc;
	    if (botnbc != ci.botnbc)
		return botnbc - ci.botnbc;
	    if (topncc != ci.topncc)
		return topncc - ci.topncc;
	    if (botncc != ci.botncc)
		return botncc - ci.botncc;
	    for (int i = 0; i < topnbc; i++)
		if (topConnectedComponent[i] != ci.topConnectedComponent[i])
		    return topConnectedComponent[i] - ci.topConnectedComponent[i];
	    for (int i = 0; i < botnbc; i++)
		if (botConnectedComponent[i] != ci.botConnectedComponent[i])
		    return botConnectedComponent[i] - ci.botConnectedComponent[i];
	    int i = top.compareTo(ci.top);
	    if (i != 0)
		return i;
	    i = middle.compareTo(ci.middle);
	    if (i != 0)
		return i;
	    return bottom.compareTo(ci.bottom);
	}
    }

    private class ComposeOutput {
	CannedCobordism out;
	public ComposeOutput(CannedCobordism a, CannedCobordism b) {
	    CannedCobordism bot = new CannedCobordism(a);
	    bot.genus = new int[bot.ncc];
	    bot.dots = new int[bot.ncc];
	    for (int i = 0; i < bot.ncc; i++) {
		bot.genus[i] = 1 << (i + 10);
		bot.dots[i] = 1 << i;
	    }
	    CannedCobordism top = new CannedCobordism(b);
	    top.genus = new int[top.ncc];
	    top.dots = new int[top.ncc];
	    for (int i = 0; i < top.ncc; i++) {
		top.genus[i] = 1 << (i + 10 + bot.ncc);
		top.dots[i] = 1 << (i + bot.ncc);
	    }
	    out = bot.compose2(top);
	}

	public ComposeOutput(CannedCobordism a, int astart,
			     CannedCobordism b, int bstart, int nc) {
	    CannedCobordism aa = new CannedCobordism(a);
	    aa.genus = new int[a.ncc];
	    aa.dots = new int[a.ncc];
	    for (int i = 0; i < a.ncc; i++) {
		aa.genus[i] = 1 << (i + 10);
		aa.dots[i] = 1 << i;
	    }
	    CannedCobordism bb = new CannedCobordism(b);
	    bb.genus = new int[b.ncc];
	    bb.dots = new int[b.ncc];
	    for (int i = 0; i < b.ncc; i++) {
		bb.genus[i] = 1 << (i + 10 + a.ncc);
		bb.dots[i] = 1 << (i + a.ncc);
	    }
	    out = aa.compose2(astart, bb, bstart, nc);
	}

	// assumes a and b are appropriate CannedCobordisms
	public CannedCobordism get(CannedCobordism a, CannedCobordism b) {
	    CannedCobordism ret = new CannedCobordism(out);
	    ret.hpower = a.hpower + b.hpower;
	    ret.genus = new int[ret.ncc];
	    ret.dots = new int[ret.ncc];
	    for (int i = 0; i < ret.ncc; i++) {
		for (int j = 0; j < a.ncc; j++)
		    if ((out.genus[i] & (1 << (10 + j))) != 0)
			ret.genus[i] += a.genus[j];
		for (int j = 0; j < b.ncc; j++)
		    if ((out.genus[i] & (1 << (10 + a.ncc + j))) != 0)
			ret.genus[i] += b.genus[j];
		ret.genus[i] += out.genus[i] & ((1 << 10) - 1);
		for (int j = 0; j < a.ncc; j++)
		    if ((out.dots[i] & (1 << j)) != 0)
			ret.dots[i] += a.dots[j];
		for (int j = 0; j < b.ncc; j++)
		    if ((out.dots[i] & (1 << (j + a.ncc))) != 0)
			ret.dots[i] += b.dots[j];
	    }
	    return ret;
	}
    }

    private class HComposeInput implements Comparable {
	int an, bn;
	Cap atop, abottom, btop, bbottom;
	int anbc, bnbc, ancc, bncc;
	int aConnectedComponent[], bConnectedComponent[];
	int astart, bstart, nc;
	public HComposeInput(CannedCobordism a, int astart,
			     CannedCobordism b, int bstart, int nc) {
	    an = a.n; bn = b.n;
	    atop = a.top; abottom = a.bottom;
	    btop = b.top; bbottom = b.bottom;
	    anbc = a.nbc; ancc = a.ncc;
	    bnbc = b.nbc; bncc = b.ncc;
	    aConnectedComponent = a.connectedComponent;
	    bConnectedComponent = b.connectedComponent;
	    this.astart = astart; this.bstart = bstart; this.nc = nc;
	}

	public int compareTo(Object o) {
	    HComposeInput ci = (HComposeInput) o;
	    if (astart != ci.astart)
		return astart - ci.astart;
	    if (bstart != ci.bstart)
		return bstart - ci.bstart;
	    if (nc != ci.nc)
		return nc - ci.nc;
	    if (an != ci.an)
		return an - ci.an;
	    if (bn != ci.bn)
		return bn - ci.bn;
	    if (anbc != ci.anbc)
		return anbc - ci.anbc;
	    if (bnbc != ci.bnbc)
		return bnbc - ci.bnbc;
	    if (ancc != ci.ancc)
		return ancc - ci.ancc;
	    if (bncc != ci.bncc)
		return bncc - ci.bncc;
	    for (int i = 0; i < anbc; i++)
		if (aConnectedComponent[i] != ci.aConnectedComponent[i])
		    return aConnectedComponent[i] - ci.aConnectedComponent[i];
	    for (int i = 0; i < bnbc; i++)
		if (bConnectedComponent[i] != ci.bConnectedComponent[i])
		    return bConnectedComponent[i] - ci.bConnectedComponent[i];
	    int i = atop.compareTo(ci.atop);
	    if (i != 0) return i;
	    i = btop.compareTo(ci.btop);
	    if (i != 0) return i;
	    i = abottom.compareTo(ci.abottom);
	    if (i != 0) return i;
	    return bbottom.compareTo(ci.bbottom);
	}
    }
}
