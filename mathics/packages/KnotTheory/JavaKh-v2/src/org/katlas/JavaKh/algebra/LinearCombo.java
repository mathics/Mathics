package org.katlas.JavaKh.algebra;

import java.util.Set;


public interface LinearCombo<R extends Ring<R>, O, Mor extends Morphism<O, Mor>, LinearMor extends LinearMorphism<R, O, LinearMor>> extends LinearMorphism<R, O, LinearMor> {
	// ooof, generics get confusing. See the interface LCCC for an example of how to use this.
	
	public abstract int numberOfTerms();
	
	public abstract R firstCoefficient();
	public abstract Mor firstTerm();

	public abstract Set<Mor> terms();
	public abstract R getCoefficient(Mor term);
	
//	public abstract LinearMor add(Mor m, int num);
	public abstract LinearMor add(Mor m, R r);
	
//	public abstract LinearMor singleTermLinearCombo(Mor mor, R r);
//	public abstract LinearMor flexibleZeroLinearCombo(O source, O target);
//	public abstract LinearMor fixedZeroLinearCombo(O source, O target);
//
	LinearMor compact();
}