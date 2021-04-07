package org.katlas.JavaKh.obsolete;

import java.util.Collections;
import java.util.Set;

import org.katlas.JavaKh.algebra.LinearCombo;
import org.katlas.JavaKh.algebra.Morphism;
import org.katlas.JavaKh.algebra.Ring;
import org.katlas.JavaKh.algebra.implementations.AbstractLinearMorphism;
import org.katlas.JavaKh.algebra.rings.Rings;

public abstract class SingleTermLinearCombo<R extends Ring<R>, O, Mor extends Morphism<O, Mor>, LinearMor extends LinearCombo<R, O, Mor, LinearMor>>
extends	AbstractLinearMorphism<R, O, LinearMor>
implements LinearCombo<R, O, Mor, LinearMor> {

	protected final R coefficient;
	protected final Mor mor;
	
	public SingleTermLinearCombo(Mor cc, R coefficient) {
		assert cc != null;
		assert coefficient != null;
		this.mor = cc;
		this.coefficient = coefficient;
	}

	public LinearMor add(Mor m, int num) {
		Rings<R> ring = Rings.current();
		return add(m, ring.createInstance(num));
	}

	public LinearMor add(Mor m, R num) {
		if(mor.equals(m)) {
			return singleTermLinearCombo(mor, coefficient.add(num)).compact();
		} else {
			assert source().equals(m.source());
			assert target().equals(m.target());
			LinearMor result = flexibleZeroLinearCombo(source(), target());
			result = result.add(mor, coefficient);
			result = result.add(m, num);
			return result.compact();
		}
	}

	public R firstCoefficient() {
		return coefficient;
	}

	public Mor firstTerm() {
		return mor;
	}

	public int numberOfTerms() {
		return coefficient.isZero() ? 0 : 1;
	}

	public boolean isZero() {
		return coefficient.isZero();
	}
	
	@SuppressWarnings("unchecked")
	public LinearMor add(LinearMor m) {
		assert source().equals(m.source());
		
		if(m.numberOfTerms() == 0) {
			return (LinearMor)this;
		} else if (m.numberOfTerms() == 1) {
			if(mor.equals(m.firstTerm())) {
				R sum = coefficient.add(m.firstCoefficient());
				return (sum.isZero()) ? fixedZeroLinearCombo(source(), target()) : singleTermLinearCombo(mor, sum);
			} 
		}
		// otherwise, create a new LCCC
		LinearMor result = flexibleZeroLinearCombo(source(), target());
		result = result.add(m);
		return result.add(mor, coefficient);
	}

	public LinearMor multiply(int r) {
		Rings<R> ring = Rings.current();
		return multiply(ring.createInstance(r));
	}

	public LinearMor multiply(R r) {
		return singleTermLinearCombo(mor, coefficient.multiply(r)).compact();
	}

	public LinearMor compose(LinearMor m) {
		if(m.numberOfTerms() == 0) {
			return fixedZeroLinearCombo(m.source(), target());
		} else if(m.numberOfTerms() == 1) {
			return singleTermLinearCombo(mor.compose(m.firstTerm()), coefficient.multiply(m.firstCoefficient())).compact();
		} else {
			LinearMor result = flexibleZeroLinearCombo(m.source(), target());
			for(Mor mor : m.terms()) {
				result.add(this.mor.compose(mor), coefficient.multiply(m.getCoefficient(mor)));
			}
			return result.compact();
		}
	}

	public R getCoefficient(Mor term) {
		if(term.equals(mor)) {
			return coefficient;
		} else {
			Rings<R> ring = Rings.current();
			return ring.ZERO;
		}
	}


	public Set<Mor> terms() {
		return Collections.singleton(mor);
	}
	
	public O source() {
		return mor.source();
	}

	public O target() {
		return mor.target();
	}

	abstract public LinearMor fixedZeroLinearCombo(O source, O target);
	abstract public LinearMor singleTermLinearCombo(Mor mor, R r);
	abstract public LinearMor flexibleZeroLinearCombo(O source, O target);

	@SuppressWarnings("unchecked")
	public LinearMor compact() {
		if(coefficient.isZero()) {
			return fixedZeroLinearCombo(source(), target());
		} else {
			return (LinearMor)this;
		}
	}

	
}
