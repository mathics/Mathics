package org.katlas.JavaKh.rows;

import java.util.Iterator;
import java.util.Set;
import java.util.TreeSet;

import net.tqft.iterables.AbstractIterator;

public class MaskedArrayRow<F> implements MatrixRow<F> {

	/**
	 * 
	 */
	private static final long serialVersionUID = -5061272587543177870L;
	int size;
	Object[] values;
	final Set<Integer> deletedIndexes;
	
	public MaskedArrayRow(int size) {
		this.size = size;
		values = new Object[size];
		deletedIndexes = new TreeSet<Integer>();
	}

	private int adjustKey(int key) {
		for(int i : deletedIndexes) {
			if(i <= key) ++key;
		}
		return key;
	}
	
	public void clear() {
		size = 0;
		deletedIndexes.clear();
	}

	public void compact() {
		// TODO
	}

	public boolean containsKey(int key) {
		assert key > -1;
		return key < size && values[adjustKey(key)] != null;
	}

	public void decrementIndexesAbove(int key) {
		deletedIndexes.add(adjustKey(key));
		--size;
	}

	@SuppressWarnings("unchecked")
	public F get(int key) {
		return (F)values[adjustKey(key)];
	}
	
	public Iterable<Integer> keys() {
		return new Iterable<Integer>() {

			public Iterator<Integer> iterator() {
				return new AbstractIterator<Integer>() {
					int i = 0;

					@Override
					public boolean hasNext() {
						while (i < size && values[adjustKey(i)] == null)
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
		values[adjustKey(key)] = f;
	}

	public void putLast(int key, F f) {
		put(key, f);
	}

	public void remove(int key) {
		put(key, null);
	}

}
