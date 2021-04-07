package org.katlas.JavaKh.utils;

public class TrivialCache<E> implements Cache<E> {

	private transient long checks = 0;
	
	public E cache(E e) {
		++checks;
		return e;
	}

	public void clear() {
	}

	public int size() {
		return 0;
	}

	public long getNumberOfHits() {
		return 0;
	}

	public long getNumberOfChecks() {
		return checks;
	}

	
}
