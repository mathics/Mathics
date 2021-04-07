/**
 * 
 */
package org.katlas.JavaKh.rows;

import java.io.Serializable;


public interface MatrixRow<F> extends Serializable {

	void decrementIndexesAbove(int key);
	
	void put(int key, F f);
	
	/*
	 * putLast is functionally the same as put, but it's allowed to fail (preferably using "assert")
	 * if the key is strictly less than a pre-existing key.
	 */
	void putLast(int key, F f);
	
	Iterable<Integer> keys();
	
	F get(int key);
	
	boolean containsKey(int key);
	
	void remove(int key);
	
	void clear();
	
	void compact();

}