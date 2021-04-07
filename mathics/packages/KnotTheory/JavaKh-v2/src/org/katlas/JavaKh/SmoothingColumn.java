package org.katlas.JavaKh;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

public class SmoothingColumn implements Serializable {
    /**
	 * 
	 */
	private static final long serialVersionUID = -4287954448286736780L;
	int n;
    List<Cap> smoothings;
    List<Integer> numbers;

    public SmoothingColumn(int n) {
	this.n = n;
	smoothings = new ArrayList<Cap>(n);
	numbers = new ArrayList<Integer>(n);
	for(int i = 0; i < n ; ++i) {
		smoothings.add(null);
		numbers.add(0);
	}
    }

    public SmoothingColumn(SmoothingColumn s) {
    	// make a copy
    	this.n = s.n;
    	this.smoothings = new ArrayList<Cap>(s.smoothings);
    	this.numbers = new ArrayList<Integer>(s.numbers);
	}

	public boolean nonNull() {
    	for(Cap cap : smoothings) {
    		if(cap == null) return false;
    	}
    	return true;
    }
    
    public boolean equals(Object o) {
	if (!(o instanceof SmoothingColumn))
	    return false;
	SmoothingColumn sc = (SmoothingColumn) o;
	if (n != sc.n)
	    return false;
	if(!smoothings.equals(sc.smoothings))
	    return false;
	if (!numbers.equals(sc.numbers))
	    return false;
	return true;
    }

    public String toString() {
    	StringBuilder sb = new StringBuilder();
    	if(n > 0) {
    		sb.append("q^(" + numbers.get(0) + ")*" + smoothings.get(0).toString());
    	}
    	for(int i = 1; i < n; ++i) {
        	sb.append(" + q^(" + numbers.get(i) + ")*" + smoothings.get(i).toString());    		
    	}
    	return sb.toString();
    }
    
    public void printNumbers() {
	for (int i = 0; i < n; i++)
	    System.out.print(" " + numbers.get(i));
	System.out.println();
    }
}
