/*
 * Created on Oct 30, 2005
 */
package net.tqft.iterables;

import java.util.Iterator;

public abstract class ForgetfulIterableBundle<S, T, U extends T> implements Iterable<T> {

	private final Iterable<S> baseIterable;

	public ForgetfulIterableBundle(Iterable<S> baseIterable) {
		this.baseIterable = baseIterable;
	}

	protected abstract Iterable<U> buildNewFibreIterable(S s);

	public Iterator<T> iterator() {

		return new ForgetfulIteratorBundle<S, T, U>(baseIterable.iterator()) {
			@Override
			protected Iterator<U> buildNewFibreIterator(S s) {
				return buildNewFibreIterable(s).iterator();
			}
		};
	}
}
