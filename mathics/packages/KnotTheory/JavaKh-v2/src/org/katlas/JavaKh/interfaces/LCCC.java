package org.katlas.JavaKh.interfaces;

import org.katlas.JavaKh.Cap;
import org.katlas.JavaKh.algebra.LinearCombo;
import org.katlas.JavaKh.algebra.Ring;

public interface LCCC<R extends Ring<R>> extends LinearCombo<R, Cap, CannedCobordism, LCCC<R>> {

	public LCCC<R> compose(int start, CannedCobordism cc, int cstart, int nc,
			boolean reverse);

	public LCCC<R> reduce();

	public LCCC<R> reduceWithH();

	public LCCC<R> finalizeH();

	
}
