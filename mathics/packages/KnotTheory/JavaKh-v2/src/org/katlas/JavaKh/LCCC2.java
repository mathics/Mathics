package org.katlas.JavaKh;

import java.util.AbstractSet;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import org.katlas.JavaKh.algebra.Ring;
import org.katlas.JavaKh.algebra.rings.Rings;
import org.katlas.JavaKh.interfaces.CannedCobordism;
import org.katlas.JavaKh.interfaces.LCCC;

public class LCCC2<R extends Ring<R>> implements LCCC<R> {

	final CannedCobordism term;
	final R coefficient;
	final LCCC2<R> next;

	private LCCC2(CannedCobordism term, R coefficient, LCCC2<R> next) {
		assert term != null;
		assert !coefficient.isZero();
		this.term = term;
		this.coefficient = coefficient;
		this.next = next;
	}

	public LCCC2(CannedCobordism term, R coefficient) {
		this(term, coefficient, null);
	}

	public LCCC2<R> compose(int start, CannedCobordism cc, int cstart, int nc,
			boolean reverse) {
		CannedCobordism composition = term.compose(start, cc, cstart, nc);

		if (reverse) {
				composition = cc.compose(cstart, term, start, nc);
		} else {
				composition = term.compose(start, cc, cstart, nc);
		}
		
		return new LCCC2<R>(composition, coefficient, next == null ? null : next.compose(start, cc,
				cstart, nc, reverse));
	}

//	public LCCC2<R> add(CannedCobordism cc, int r) {
//		Rings<R> ring = Rings.current();
//		return add(cc, ring.createInstance(r));
//	}

	public LCCC2<R> add(CannedCobordism cc, R r) {
		if (term.equals(cc)) {
			R sum = coefficient.add(r);
			if(sum.isZero()) {
				return next;
			} else {
				return new LCCC2<R>(term, sum, next);
			}
		} else {
			return new LCCC2<R>(term, coefficient, next == null ? null : next.add(cc, r));
		}
	}

	public R firstCoefficient() {
		return coefficient;
	}

	public CannedCobordism firstTerm() {
		return term;
	}

	public R getCoefficient(CannedCobordism cc) {
		if (term.equals(cc)) {
			return coefficient;
		} else {
			if (next == null) {
				Rings<R> ring = Rings.current();
				return ring.ZERO;
			} else {
				return next.getCoefficient(cc);
			}
		}
	}

	public int numberOfTerms() {
		if (next == null) {
			return 1;
		} else {
			return 1 + next.numberOfTerms();
		}
	}

	public Set<CannedCobordism> terms() {
		final LCCC2<R> this$ = this;
		return new AbstractSet<CannedCobordism>() {

			@Override
			public Iterator<CannedCobordism> iterator() {
				return new Iterator<CannedCobordism>() {
					LCCC2<R> pointer = this$;

					public boolean hasNext() {
						return pointer != null;
					}

					public CannedCobordism next() {
						CannedCobordism result = pointer.term;
						pointer = pointer.next;
						return result;
					}

					public void remove() {
						throw new UnsupportedOperationException();
					}
				};
			}

			@Override
			public int size() {
				return numberOfTerms();
			}

		};
	}

	public LCCC2<R> add(LCCC<R> m) {
		LCCC2<R> result = this;
		for (CannedCobordism cc : m.terms()) {
			result = result.add(cc, m.getCoefficient(cc));
		}
		return result;
	}

	public boolean isZero() {
		return coefficient.isZero() && (next == null || next.isZero());
	}

//	public LCCC<R> multiply(int r) {
//		Rings<R> ring = Rings.current();
//		return multiply(ring.createInstance(r));
//	}

	public LCCC2<R> multiply(R r) {
		if(r.isZero()) return null;
		return new LCCC2<R>(term, coefficient.multiply(r), next == null ? null
				: next.multiply(r));
	}

	public LCCC2<R> compose(LCCC<R> m) {
		LCCC2<R> result = null;
		for (CannedCobordism cc : m.terms()) {
			CannedCobordism composition = term.compose(cc);
			R r = coefficient.multiply(m.getCoefficient(cc));
			if (result == null) {
				result = new LCCC2<R>(composition, r);
			} else {
				result = result.add(composition, r);
			}
		}
		if (next != null) {
			result = result.add(next.compose(m));
		}
		return result;
	}

	public Cap source() {
		return term.source();
	}

	public Cap target() {
		return term.target();
	}

	public LCCC2<R> reduce() {

		if (JavaKh.using_h)
			return reduceWithH();

		// if (alreadyReduced) {
		// return this;
		// }

		LCCC2<R> ret = null;

		if (!(term instanceof CannedCobordismImpl)) {
			throw new UnsupportedOperationException();
		}
		CannedCobordismImpl cc = (CannedCobordismImpl) term;

		R num = coefficient;

		cc.reverseMaps();
		byte dots[] = new byte[cc.nbc];
		byte genus[] = CannedCobordismImpl.zeros[cc.nbc];
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
				dots[cc.boundaryComponents[i][0]] = (byte) (cc.dots[i] + cc.genus[i]);
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
					for (int j = 0; j < cc.boundaryComponents[i].length; j++)
						dots[cc.boundaryComponents[i][j]] = 1;
				} else {
					// cc.bC[i].length choices
					// use dots to cancel out all the factors, except
					// one which is cancelled by the torus
					// do these last
					moreWork[nmoreWork++] = i;
				}
			}
		if (!kill) {
			byte neckCutting[][] = new byte[1][];
			neckCutting[0] = dots;
			for (int i = 0; i < nmoreWork; i++) {
				int concomp = moreWork[i];
				int nbc = cc.boundaryComponents[concomp].length;
				byte newarr[][] = new byte[neckCutting.length * nbc][cc.nbc];
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
			byte connectedComponent[] = CannedCobordismImpl.counting[cc.nbc];
			for (int i = 0; i < neckCutting.length; i++) {
				CannedCobordismImpl newcc = new CannedCobordismImpl(term
						.source(), term.target());
				// IMPORTANT!!! in order for them to safely share arrays
				// CannedCobordisms must be treated as immutable
				newcc.connectedComponent = connectedComponent;
				newcc.ncc = newcc.nbc;
				newcc.genus = genus;
				newcc.dots = neckCutting[i];
				ret = (ret == null) ? new LCCC2<R>(CannedCobordismImpl.cobordismCache.cache(newcc), num) : ret.add(CannedCobordismImpl.cobordismCache.cache(newcc),
						num);
			}
		}

		if (next != null) {
			ret = (ret == null) ? next.reduce() : ret.add(next.reduce());
		}

		return ret;
	}

	public LCCC2<R> reduceWithH() {
		// if (alreadyReduced) {
		// return this;
		// }

		Rings<R> ring = Rings.current();

		LCCC2<R> ret = null;

		if (!(term instanceof CannedCobordismImpl)) {
			throw new UnsupportedOperationException();
		}
		CannedCobordismImpl cc = (CannedCobordismImpl) term;

		R num = coefficient;
		cc.reverseMaps();
		byte dots[] = new byte[cc.nbc];
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
					for (int j = 0; j < cc.boundaryComponents[i].length; j++)
						dots[cc.boundaryComponents[i][j]] = 1;
					hpow += cc.dots[i] + cc.genus[i] - 1;
				} else
					moreWork[nmoreWork++] = i;
			}
		if (!kill) {
			byte nCdots[][] = new byte[1][];
			int nChpow[] = new int[1];
			List<R> nCnum = new ArrayList<R>(1);
			nCnum.add(num);
			nCdots[0] = dots;
			nChpow[0] = hpow;
			for (int i = 0; i < nmoreWork; i++) {
				int concomp = moreWork[i];
				int nbc = cc.boundaryComponents[concomp].length;
				assert cc.dots[concomp] == 0;
				byte newdots[][] = new byte[nCdots.length << nbc][cc.nbc];
				int newhpow[] = new int[nChpow.length << nbc];
				List<R> newnum = new ArrayList<R>(nCnum.size() << nbc);
				for (int s = 0; s < nCnum.size() << nbc; ++s) {
					newnum.add(ring.ZERO);
				}
				assert newnum.size() == nCnum.size() << nbc;
				for (int j = 0; j < nCdots.length; j++) {
					for (int k = 0; k < (1 << nbc); k++) {
						int idx = (j << nbc) + k;
						System.arraycopy(nCdots[j], 0, newdots[idx], 0, cc.nbc);
						newhpow[idx] = nChpow[j];
						newnum.set(idx, nCnum.get(j));
						int nzeros = 0;
						for (int l = 0; l < nbc; l++)
							if ((k & (1 << l)) == 0) {
								newdots[idx][cc.boundaryComponents[concomp][l]] = 0;
								nzeros++;
							} else
								newdots[idx][cc.boundaryComponents[concomp][l]] = 1;
						R nmul = ring.ZERO;
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
								nmul = nmul.add(ring.createInstance(n));
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
								nmul = nmul.add(ring.createInstance(n));
							}
						}
						newhpow[idx] += hmod;
						newnum.set(idx, newnum.get(idx).multiply(nmul));
					}
				}
				nCdots = newdots;
				nChpow = newhpow;
				nCnum = newnum;
			}
			for (int i = 0; i < nCdots.length; i++) {
				CannedCobordismImpl newcc = new CannedCobordismImpl(term
						.source(), term.target());
				newcc.connectedComponent = CannedCobordismImpl.counting[newcc.nbc];
				newcc.ncc = newcc.nbc;
				newcc.genus = CannedCobordismImpl.zeros[cc.nbc];
				newcc.dots = nCdots[i];
				newcc.hpower = nChpow[i];
				ret = (ret == null ? new LCCC2<R>(CannedCobordismImpl.cobordismCache.cache(newcc), nCnum.get(i)) : ret
						.add(CannedCobordismImpl.cobordismCache.cache(newcc), nCnum.get(i)));
			}
		}

		if (next != null) {
			ret = (ret == null) ? next.reduce() : ret.add(next.reduce());
		}

		return ret;
	}

	public LCCC<R> finalizeH() {
return null;
		
		//		
//		assert source.n == 2 && source.ncycles == 0 && target.n == 2
//				&& target.ncycles == 0;
//		LCCC<R> ret = new LCCCMap<R>(source, target);
//		CannedCobordismImpl cc = CannedCobordismImpl.isomorphism(source);
//		boolean hset = false;
//		for (CannedCobordism iocc : coefficients.keySet()) {
//			if (!(iocc instanceof CannedCobordismImpl)) {
//				throw new UnsupportedOperationException();
//			}
//			CannedCobordismImpl occ = (CannedCobordismImpl) iocc;
//
//			if (!coefficients.get(occ).isZero()) {
//				if (!hset)
//					cc.hpower = occ.hpower + occ.dots[0] + occ.genus[0];
//				else if (cc.hpower != occ.hpower + occ.dots[0] + occ.genus[0])
//					throw new AssertionError();
//				ret = ret.add(cc, coefficients.get(occ));
//			}
//		}
//		return ret.compact();
	}

	public String toString() {
		StringBuilder sb = new StringBuilder();
		Iterator<CannedCobordism> i = terms().iterator();
		if(i.hasNext()) {
			CannedCobordism term = i.next();
			sb.append(getCoefficient(term));
			sb.append(" * ");
			sb.append(term);
		}
		while(i.hasNext()) {
			CannedCobordism term = i.next();
			sb.append(" + ");
			sb.append(getCoefficient(term));
			sb.append(" * ");
			sb.append(term);
		}
		return sb.toString();
	}

	public LCCC<R> compact() {
		if(numberOfTerms() == 0) return null;
		else return this;
	}
	
}
