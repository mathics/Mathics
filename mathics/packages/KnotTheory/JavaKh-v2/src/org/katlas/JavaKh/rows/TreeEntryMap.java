package org.katlas.JavaKh.rows;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

public class TreeEntryMap<F> implements MatrixRow<F>, Serializable {
	/**
		 * 
		 */
	private static final long serialVersionUID = 7040588247299310395L;

	Map<Integer, F> map = new TreeMap<Integer, F>();

	public void compact() {
	}

	public boolean containsKey(int k) {
		return map.containsKey(k);
	}

	public F get(int key) {
		return map.get(key);
	}

	public Iterable<Integer> keys() {
		return map.keySet();
	}

	public void put(int key, F lc) {
		map.put(key, lc);
	}

	public void remove(int key) {
		map.remove(key);
	}

	public void decrementIndexesAbove(int key) {
		List<Integer> targetIndexes = new ArrayList<Integer>();
		for (int k : map.keySet()) {
			if (k > key) {
				targetIndexes.add(k);
			}
		}
		for (int k : targetIndexes) {
			map.put(k - 1, map.get(k));
			map.remove(k);
		}
	}

	public void clear() {
		map.clear();
	}

	public void putLast(int key, F f) {
		put(key, f);
	}

}
