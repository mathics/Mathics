package org.katlas.JavaKh.algebra.implementations;

import java.util.Iterator;

import org.katlas.JavaKh.algebra.LinearCombo;
import org.katlas.JavaKh.algebra.LinearMorphism;
import org.katlas.JavaKh.algebra.Morphism;
import org.katlas.JavaKh.algebra.Ring;

public abstract class AbstractLinearCombo<R extends Ring<R>, O, Mor extends Morphism<O, Mor>, LinearMor extends LinearMorphism<R, O, LinearMor>> implements LinearCombo<R, O, Mor, LinearMor> {

//	protected final O source, target;
//	
//	public AbstractLinearCombo(O source, O target) {
//		this.source = source;
//		this.target = target;
//	}
	
	public R firstCoefficient() {
		return getCoefficient(firstTerm());
	}

	public Mor firstTerm() {
		return terms().iterator().next();
	}

	public boolean isZero() {
		return terms().isEmpty();
	}
	
	public int numberOfTerms() {
		return terms().size();
	}

	public String toString() {
		StringBuilder sb = new StringBuilder();
		Iterator<Mor> i = terms().iterator();
		if(i.hasNext()) {
			Mor term = i.next();
			sb.append(getCoefficient(term));
			sb.append(" * ");
			sb.append(term);
		}
		while(i.hasNext()) {
			Mor term = i.next();
			sb.append(" + ");
			sb.append(getCoefficient(term));
			sb.append(" * ");
			sb.append(term);
		}
		return sb.toString();
	}
	
//	@SuppressWarnings("unchecked")
//	public	LinearMor compact() {
////		return (LinearMor)this;
//		
//		if(numberOfTerms() == 0) {
//			return fixedZeroLinearCombo(source(), target());
//		} else if (numberOfTerms() == 1) {
//			return singleTermLinearCombo(firstTerm(), firstCoefficient());
//		} else {
//			return (LinearMor)this;
//		}	
//	}
	
}
