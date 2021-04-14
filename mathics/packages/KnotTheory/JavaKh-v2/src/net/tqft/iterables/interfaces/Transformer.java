package net.tqft.iterables.interfaces;


public interface Transformer<S, T> {
	
	public abstract T evaluate(S s);
	
}
