package net.tqft.iterables;

import java.util.Iterator;

public abstract class IteratorBundle<S, T> extends ForgetfulIteratorBundle<S, T, T> {

	public IteratorBundle(Iterator<S> baseIterator) {
		super(baseIterator);
	}

}
