package org.katlas.JavaKh.rows;

import java.util.Iterator;

import net.tqft.iterables.AbstractIterator;

public class PackedArrayRow<F> implements MatrixRow<F> {

	/**
	 * 
	 */
	private static final long serialVersionUID = 7270137819666353651L;

	private static final int INITIAL_SIZE = 1;

	private transient int pointer = -1;
	private int size;
	private int[] keys = new int[INITIAL_SIZE];
	private Object[] values = new Object[INITIAL_SIZE];

	public void clear() {
		keys = new int[INITIAL_SIZE];
		values = new Object[INITIAL_SIZE];
	}

	private void extendArrays() {
		int[] newKeys = new int[keys.length * 2];
		Object[] newValues = new Object[keys.length * 2];
		System.arraycopy(keys, 0, newKeys, 0, size);
		System.arraycopy(values, 0, newValues, 0, size);
		keys = newKeys;
		values = newValues;
	}

	public void compact() {
		if(size == keys.length) return;
		int[] newKeys = new int[size];
		Object[] newValues = new Object[size];
		System.arraycopy(keys, 0, newKeys, 0, size);
		System.arraycopy(values, 0, newValues, 0, size);
		keys = newKeys;
		values = newValues;
	}

	public boolean containsKey(int key) {
		if (pointer != -1 && keys[pointer] == key)
			return true;
		for (int i = 0; i < size; ++i) {
			if (keys[i] == key) {
				pointer = i;
				return true;
			}
			if (keys[i] > key)
				return false;
		}
		return false;
	}

	public void decrementIndexesAbove(int key) {
		for (int i = (pointer != -1 && keys[pointer] < key) ? pointer : 0; i < keys.length; ++i) {
			if (keys[i] > key) {
				--keys[i];
			}
		}
	}

	@SuppressWarnings("unchecked")
	public F get(int key) {
		int i = 0;
		if (pointer != -1) {
			if (keys[pointer] == key)
				return (F) values[pointer];
			else if (keys[pointer] < key)
				i = pointer;
		}
		while (i < size && keys[i] < key)
			++i;
		if (i == size)
			return null;
		if (keys[i] == key) {
			pointer = i;
			return (F) values[i];
		} else {
			return null;
		}
	}

	public Iterable<Integer> keys() {
		return new Iterable<Integer>() {
			public Iterator<Integer> iterator() {
				return new AbstractIterator<Integer>() {
					int i = 0;

					public boolean hasNext() {
						return i < size;
					}

					public Integer returnNext() {
						pointer = i;
						return keys[i++];
					}
				};
			}
		};
	}

	public void put(int key, F f) {
		if(f == null) {
			remove(key);
			return;
		}
		
		int i = 0;
		if (pointer != -1) {
			if (keys[pointer] == key) {
				values[pointer] = f;
				return;
			} else if (keys[pointer] < key) {
				i = pointer;
			}
		}

		while (i < size && keys[i] < key)
			++i;

		if (i == size) {
			if (keys.length == size)
				extendArrays();
			keys[size] = key;
			values[size] = f;
			pointer = size;
			++size;
			return;
		}

		if (keys[i] == key) {
			pointer = i;
			values[i] = f;
			return;
		} else {
			// key < key[i]
			if (keys.length == size)
				extendArrays();
			for (int j = size - 1; j >= i; --j) {
				keys[j + 1] = keys[j];
				values[j + 1] = values[j];
			}
			pointer = i;
			keys[i] = key;
			values[i] = f;
			++size;
		}
	}

	public void remove(int key) {
		int i = 0;
		if (pointer != -1 && keys[pointer] == key) {
			i = pointer;
		} else {
			while (keys[i] < key && i < size)
				++i;
			if (keys[i] > key || i == size)
				return;
		}
		while (i < size - 1) {
			keys[i] = keys[i + 1];
			values[i] = values[i + 1];
			++i;
		}
		values[i] = null;
		pointer = -1;
		--size;
	}

	public void putLast(int key, F f) {
		if(f == null) {
			remove(key);
			return;
		}
		
		if (keys.length == size)
			extendArrays();
		keys[size] = key;
		values[size] = f;
		++size;
	}

}
