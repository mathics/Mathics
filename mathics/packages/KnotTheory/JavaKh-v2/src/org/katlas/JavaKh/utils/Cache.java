package org.katlas.JavaKh.utils;

public interface Cache<E> {
	
	public E cache(E e);
	public int size();
	public void clear();
	public long getNumberOfChecks();
	public long getNumberOfHits();
	
}
