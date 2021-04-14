package org.katlas.JavaKh.interfaces;

import org.katlas.JavaKh.Cap;
import org.katlas.JavaKh.algebra.Morphism;

public interface CannedCobordism extends Morphism<Cap, CannedCobordism> {

	public CannedCobordism compose(int start, CannedCobordism cc, int cstart, int nc);
	public boolean isIsomorphism();
	
	public void reverseMaps(); // planning to get rid of this!
	
}
