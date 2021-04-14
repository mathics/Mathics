package org.katlas.JavaKh.obsolete;

import java.io.Serializable;
import java.util.Set;

import org.katlas.JavaKh.Cap;
import org.katlas.JavaKh.LCCCMap;
import org.katlas.JavaKh.algebra.Ring;
import org.katlas.JavaKh.interfaces.CannedCobordism;
import org.katlas.JavaKh.interfaces.LCCC;

/*
 * WARNING
 * SingleTermLCCC is somehow horribly broken. Don't use it.
 * You've been warned.
 */


public class SingleTermLCCC<R extends Ring<R>>
//	extends	SingleTermLinearCombo<R, Cap, CannedCobordism, LCCC<R>>
	implements LCCC<R>, Serializable {

	/**
	 * 
	 */
	private static final long serialVersionUID = -28865833270013691L;
	
	transient boolean alreadyReduced;

	private LCCC<R> inner;
	
	public SingleTermLCCC(CannedCobordism cc, R r) {
		inner = new LCCCMap<R>(cc.source(), cc.target());
		inner = inner.add(cc, r);
		unwrap();
		
		assert false;
	}

	private void unwrap() {
		if(inner instanceof SingleTermLCCC) {
			inner = ((SingleTermLCCC<R>)inner).inner;
		}
	}

//	public SingleTermLCCC(CannedCobordism cc, R coefficient) {
//		super(cc, coefficient);
////		assert false;
//	}
//
//	public LCCC<R> compose(int start, CannedCobordism cc, int cstart, int nc,
//			boolean reverse) {
//		CannedCobordism resultCC;
//		if (reverse) {
//			resultCC = cc.compose(cstart, mor, start, nc);
//		} else {
//			resultCC = mor.compose(start, cc, cstart, nc);
//		}
//		LCCC<R> result = singleTermLinearCombo(resultCC, coefficient);
//		return result;
//	}
//
//
//	@Override
//	public String toString() {
//		StringBuilder sb = new StringBuilder();
//		sb.append(coefficient);
//		sb.append(" * ");
//		sb.append(mor);
//		return sb.toString();
//	}
//
//	public LCCC<R> compact() {
//		if (coefficient.isZero()) {
//			return fixedZeroLinearCombo(source(), target());
//		} else {
//			return this;
//		}
//	}
//
//	public LCCC<R> finalizeH() {
//		return new LCCCMap<R>(this).finalizeH();
//	}
//
//	public LCCC<R> reduce() {
//		if(alreadyReduced) return this;
//		else return new LCCCMap<R>(this).reduce();
//	}
//
//	public LCCC<R> reduceWithH() {
//		if(alreadyReduced) return this;
//		else return new LCCCMap<R>(this).reduceWithH();
//	}

	public LCCC<R> fixedZeroLinearCombo(Cap source, Cap target) {
		return new ZeroLCCC<R>(source, target);
	}

	public LCCC<R> singleTermLinearCombo(CannedCobordism cc, R r) {
		return new SingleTermLCCC<R>(cc, r);
	}

	public LCCC<R> flexibleZeroLinearCombo(Cap source, Cap target) {
		return new LCCCMap<R>(source, target);
	}

	public LCCC<R> compose(int start, CannedCobordism cc, int cstart, int nc,
			boolean reverse) {
		inner = inner.compose(start, cc, cstart, nc, reverse);
		unwrap();
		return inner;
	}

	public LCCC<R> finalizeH() {
		inner = inner.finalizeH();
		unwrap();
		return inner;
	}

	public LCCC<R> reduce() {
		inner = inner.reduce();
		unwrap();
		return inner;
	}

	public LCCC<R> reduceWithH() {
		inner = inner.reduceWithH();
		unwrap();
		return inner;
	}

	public LCCC<R> add(CannedCobordism m, int num) {
		inner = inner.add(m, num);
		unwrap();
		return inner;
	}

	public LCCC<R> add(CannedCobordism m, R r) {
		inner = inner.add(m, r);
		unwrap();
		return inner;
	}

	public LCCC<R> compact() {
		inner = inner.compact();
		unwrap();
		return inner;
	}

	public R firstCoefficient() {
		return inner.firstCoefficient();
	}

	public CannedCobordism firstTerm() {
		return inner.firstTerm();
	}

	public R getCoefficient(CannedCobordism term) {
		return inner.getCoefficient(term);
	}

	public int numberOfTerms() {
		return inner.numberOfTerms();
	}

	public Set<CannedCobordism> terms() {
		return inner.terms();
	}

	public LCCC<R> add(LCCC<R> m) {
		inner = inner.add(m);
		unwrap();
		return inner;	
	}

	public boolean isZero() {
		return inner.isZero();
	}

	public LCCC<R> multiply(int r) {
		inner = inner.multiply(r);
		unwrap();
		return inner;
		}

	public LCCC<R> multiply(R r) {
		inner = inner.multiply(r);
		unwrap();
		return inner;
		}

	public LCCC<R> compose(LCCC<R> m) {
		inner = inner.compose(m);
		unwrap();
		return inner;
		}

	public Cap source() {
		return inner.source();
	}

	public Cap target() {
		return inner.target();
	}
	
}
