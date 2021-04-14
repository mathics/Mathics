package org.katlas.JavaKh.utils;
import java.io.Serializable;
import java.util.Collection;
import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.Set;


public class AlwaysEmptySet<V> implements Set<V>, Serializable {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1283740797021223218L;

	public boolean add(V arg0) {
		return true;
	}

	public boolean addAll(Collection<? extends V> arg0) {
		return true;
	}

	public void clear() {
		
	}

	public boolean contains(Object arg0) {
		return false;
	}

	public boolean containsAll(Collection<?> c) {
		return c.isEmpty();
	}

	public boolean isEmpty() {
		return true;
	}

	public Iterator<V> iterator() {
		return new Iterator<V>() {

	public boolean hasNext() {
		return false;
	}

			public V next() {
				throw new NoSuchElementException();
			}

			public void remove() {
				throw new UnsupportedOperationException();
			}
			
		};
	}

	public boolean remove(Object o) {
		return false;
	}

	public boolean removeAll(Collection<?> c) {
		return false;
	}

	public boolean retainAll(Collection<?> c) {
		return false;
	}

	public int size() {
		return 0;
	}

	public Object[] toArray() {
		return new Object[] {};
	}

	@SuppressWarnings("unchecked")
	public <T> T[] toArray(T[] a) {
		return (T[])(new Object[] {});
	}

}
