package org.katlas.JavaKh;

import java.io.Serializable;

import org.katlas.JavaKh.Cap;
import org.katlas.JavaKh.LCCCMap;
import org.katlas.JavaKh.algebra.Ring;
import org.katlas.JavaKh.algebra.implementations.ZeroLinearCombo;
import org.katlas.JavaKh.interfaces.CannedCobordism;
import org.katlas.JavaKh.interfaces.LCCC;

public class ZeroLCCC<R extends Ring<R>> extends
		ZeroLinearCombo<R, Cap, CannedCobordism, LCCC<R>> implements
		LCCC<R>, Serializable {

	/**
	 * 
	 */
	private static final long serialVersionUID = -985131448758667387L;

	ZeroLCCC() {
	}

	public LCCC<R> finalizeH() {
		return this;
	}

	public LCCC<R> reduce() {
		return this;
	}

	public LCCC<R> reduceWithH() {
		return this;
	}
	
	public LCCC<R> compose(int start, CannedCobordism cc, int cstart, int nc,
			boolean reverse) {
		assert false;
		throw new UnsupportedOperationException();
//		return new ZeroLCCC<R>(source().compose(start, cc.source(),
//				cstart, nc), target().compose(start, cc.target(), cstart, nc));
	}

	@Override
	public LCCC<R> fixedZeroLinearCombo() {
		return new ZeroLCCC<R>();
	}

	@Override
	public LCCC<R> flexibleZeroLinearCombo() {
		return new LCCCMap<R>();
	}

	@Override
	public LCCC<R> singleTermLinearCombo(CannedCobordism mor, R r) {
		/*
		 * WARNING
		 * SingleTermLCCC is somehow horribly broken. Don't use it.
		 * You've been warned.
		 */

//		return new SingleTermLCCC<R>(mor, r);
		LCCCMap<R> result = new LCCCMap<R>(mor, r);
		return result;
	}

}
