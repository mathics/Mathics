package org.katlas.JavaKh;
// sparse matrix
// based on http://www.ii.uib.no/~geirg/jaggedarrays.html
public class CobMatrix {
    SmoothingColumn source, target;
    LCCC values[][];
    int indices[][];
    int rowsizes[];

    public CobMatrix(SmoothingColumn s, SmoothingColumn t) {
	source = s;
	target = t;
	values = new LCCC[t.n][];
	indices = new int[t.n][];
	rowsizes = new int[t.n];
    }

    public void trim() {
	for (int i = 0; i < target.n; i++) {
	    if (values[i] != null && values[i].length != rowsizes[i]) {
		LCCC newval[] = new LCCC[rowsizes[i]];
		System.arraycopy(values[i], 0, newval, 0, rowsizes[i]);
		values[i] = newval;
		int newidx[] = new int[rowsizes[i]];
		System.arraycopy(indices[i], 0, newidx, 0, rowsizes[i]);
		indices[i] = newidx;
	    }
	}
    }

    // assumes matrix[i][j] is not contained in this sparse matrix
    public void append(int i, int j, LCCC lc) {
	if (lc == null || lc.n == 0)
	    return;
	if (values[i] == null || values[i].length == rowsizes[i]) {
	    int newsize;
	    if (values[i] == null || rowsizes[i] < 3)
		newsize = 4;
	    else
		newsize = rowsizes[i] * 2;
	    LCCC newval[] = new LCCC[newsize];
	    if (values[i] != null)
		System.arraycopy(values[i], 0, newval, 0, rowsizes[i]);
	    int newidx[] = new int[newsize];
	    if (indices[i] != null)
		System.arraycopy(indices[i], 0, newidx, 0, rowsizes[i]);
	    values[i] = newval;
	    indices[i] = newidx;
	}
	values[i][rowsizes[i]] = lc;
	indices[i][rowsizes[i]] = j;
	rowsizes[i]++;
    }

    public LCCC[] unpackRow(int i) {
	LCCC rowi[] = new LCCC[source.n];
	for (int j = 0; j < rowsizes[i]; j++)
	    rowi[indices[i][j]] = values[i][j];
	return rowi;
    }

    public void packRow(LCCC rowi[], int i) {
	int size = 0;
	for (int j = 0; j < source.n; j++)
	    if (rowi[j] != null && rowi[j].n != 0)
		size++;
	rowsizes[i] = size;
	values[i] = new LCCC[size];
	indices[i] = new int[size];
	for (int j = 0, k = 0; j < source.n; j++)
	    if (rowi[j] != null && rowi[j].n != 0) {
		values[i][k] = rowi[j];
		indices[i][k++] = j;
	    }
    }

    // not working right now
    public boolean equals(Object o) {
	if (!(o instanceof CobMatrix))
	    return false;
	CobMatrix cm = (CobMatrix) o;
	if (!source.equals(cm.source))
	    return false;
	if (!target.equals(cm.target))
	    return false;
	// Arrays.equals() isn't adequate because null == 0 for LCCCs
	for (int i = 0; i < target.n; i++) {
	    LCCC arowi[] = unpackRow(i);
	    LCCC browi[] = cm.unpackRow(i);
	    for (int j = 0; j < source.n; j++)
		if (arowi[j] != null) {
		    if (!arowi[j].equals(browi[j]))
			return false;
		} else if (browi[j] != null && browi[j].n != 0)
		    return false;
	}
	return true;
    }

    public CobMatrix multiply(CobMatrix cm) { // this * cm
	assert source.equals(cm.target);
	/*if (!source.equals(cm.target))
	  throw new IllegalArgumentException();*/
	CobMatrix ret = new CobMatrix(cm.source, target);
	for (int i = 0; i < target.n; i++) {
	    LCCC rowi[] = new LCCC[cm.source.n];
	    for (int j = 0; j < rowsizes[i]; j++) {
		int idx = indices[i][j];
		LCCC a = values[i][j];
		if (a != null)
		    for (int k = 0; k < cm.rowsizes[idx]; k++) {
			int cmidx = cm.indices[idx][k];
			LCCC lc = a.compose(cm.values[idx][k]);
			if (rowi[cmidx] == null)
			    rowi[cmidx] = lc;
			else
			    rowi[cmidx].add(lc);
		    }
	    }
	    int size = 0;
	    for (int j = 0; j < rowi.length; j++)
		if (rowi[j] != null && rowi[j].n != 0)
		    size++;
	    ret.rowsizes[i] = size;
	    ret.values[i] = new LCCC[size];
	    ret.indices[i] = new int[size];
	    for (int j = 0, k = 0; j < rowi.length; j++)
		if (rowi[j] != null && rowi[j].n != 0) {
		    ret.values[i][k] = rowi[j];
		    ret.indices[i][k] = j;
		    k++;
		}
	}
	return ret;
    }

    public void multiply(BaseRing n) { // modifies in place
	for (int i = 0; i < target.n; i++)
	    for(int j = 0; j < rowsizes[i]; j++)
		if (values[i][j] != null)
		    values[i][j].multiply(n);
    }

    public void add(CobMatrix cm) { // edits in place
	assert source.equals(cm.source) && target.equals(cm.target);
	for (int i = 0; i < target.n; i++) {
	    LCCC rowi[] = new LCCC[source.n];
	    for (int j = 0; j < rowsizes[i]; j++)
		rowi[indices[i][j]] = values[i][j];
	    for (int j = 0; j < cm.rowsizes[i]; j++) {
		int idx = cm.indices[i][j];
		if (rowi[idx] == null)
		    rowi[idx] = cm.values[i][j];
		else
		    rowi[idx].add(cm.values[i][j]);
	    }
	    int size = 0;
	    for (int j = 0; j < source.n; j++)
		if (rowi[j] != null && rowi[j].n != 0)
		    size++;
	    rowsizes[i] = size;
	    values[i] = new LCCC[size];
	    indices[i] = new int[size];
	    for (int j = 0, k = 0; j < source.n; j++)
		if (rowi[j] != null && rowi[j].n != 0) {
		    values[i][k] = rowi[j];
		    indices[i][k] = j;
		    k++;
		}
	}
    }

    public void reduce() { // modifies this CobMatrix in place
	for (int i = 0; i < target.n; i++)
	    for (int j = 0; j < rowsizes[i]; j++)
		if (values[i][j] != null)
		    values[i][j] = values[i][j].reduce();
    }

    public boolean isZero() {
	for (int i = 0; i < target.n; i++)
	    for (int j = 0; j < rowsizes[i]; j++)
		if (values[i][j] != null && values[i][j].n != 0)
		    return false;
	return true;
    }

    public boolean check() {
	for (int i = 0; i < target.n; i++)
	    for (int j = 0; j < rowsizes[i]; j++)
		if (values[i][j] != null)
		    if (!values[i][j].top.equals(source.smoothings[j])
			|| !values[i][j].bottom.equals(target.smoothings[i]))
			return false;
	return true;
    }

    public void print() {
	System.out.print("[");
	for (int i = 0; i < target.n; i++) {
	    LCCC rowi[] = unpackRow(i);
	    for (int j = 0; j < source.n; j++) {
		String n;
		if (rowi[j] == null || rowi[j].n == 0)
		    n = "0";
		else {
		    assert rowi[j].n == 1;
		    n = rowi[j].coefficients[0].toString();
		}
		System.out.print(n);
		if (j != source.n - 1)
		    System.out.print(",");
	    }
	    System.out.print("; ");
	}
	System.out.println("]");
    }

    public void printZeros() {
	//throw new UnsupportedOperationException();
	for (int i = 0; i < target.n; i++) {
	    LCCC rowi[] = unpackRow(i);
	    for (int j = 0; j < source.n; j++)
		if (rowi[j] == null)
		    System.out.print("0");
		else {
		    int n = rowi[j].n;
		    if (n == 1) {
			CannedCobordism cc = rowi[j].cobordisms[0];
			if (cc.isIsomorphism())
			    System.out.print("i");
			else
			    System.out.print("1");
		    } else
			System.out.print(n);
		}
	    System.out.println();
	}
    }
}
