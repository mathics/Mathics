package org.katlas.JavaKh.rows;

import java.util.Iterator;

import net.tqft.iterables.AbstractIterator;

public class ArrayRow<F> implements MatrixRow<F> {

	/**
	 * 
	 */
	private static final long serialVersionUID = 5773066111238552922L;
	int size;
	Object[] values;
	
	public ArrayRow(int size) {
		this.size = size;
		values = new Object[size];
	}

	public void clear() {
		size = 0;
	}

	public void compact() {
		Object[] newValues = new Object[size];
		System.arraycopy(values, 0, newValues, 0, size);
		values = newValues;
	}

	public boolean containsKey(int key) {
		assert key > -1;
		return key < size && values[key] != null;
	}

	public void decrementIndexesAbove(int key) {
		System.arraycopy(values, key + 1, values, key, size - key -1);
		--size;
	}

	@SuppressWarnings("unchecked")
	public F get(int key) {
		return (F)values[key];
	}
	
	public Iterable<Integer> keys() {
		return new Iterable<Integer>() {

			public Iterator<Integer> iterator() {
				return new AbstractIterator<Integer>() {
					int i = 0;

					@Override
					public boolean hasNext() {
						while (i < size && values[i] == null)
							++i;
						return i < size;
					}

					@Override
					protected Integer returnNext() {
						return i++;
					}
				};
			}

		};

	}

	public void put(int key, F f) {
		assert key < size;
		values[key] = f;
	}

	public void putLast(int key, F f) {
		put(key, f);
	}

	public void remove(int key) {
		put(key, null);
	}

}
