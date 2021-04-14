package net.tqft.iterables;

public abstract class IterableBundle<S, T> extends ForgetfulIterableBundle<S, T, T> {

	public IterableBundle(Iterable<S> baseIterable) {
		super(baseIterable);
	}

}
