package org.katlas.JavaKh.rows;

import org.katlas.JavaKh.utils.RedBlackIntegerTree;

public class RedBlackIntegerMap<F> extends RedBlackIntegerTree<F> implements MatrixRow<F> {

	/**
	 * 
	 */
	private static final long serialVersionUID = 5885667469881867107L;

	public void compact() {
	
	}

	public void putLast(int key, F f) {
		put(key, f);
	}

	@Override
	public void put(int key, F value) {
		if(value == null) {
			remove(key);
		} else {
			super.put(key, value);
		}
	}
	
	

}
