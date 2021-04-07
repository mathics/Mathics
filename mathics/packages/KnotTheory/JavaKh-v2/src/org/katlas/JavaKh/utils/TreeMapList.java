package org.katlas.JavaKh.utils;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import java.util.NavigableMap;
import java.util.TreeMap;
import java.util.Map.Entry;

// not tested, maybe incomplete

public class TreeMapList<E> implements List<E> {
	
	NavigableMap<Integer, E> map;
	int size;
	
	public TreeMapList() {
		map = new TreeMap<Integer, E>();
		size = 0;
	}
	
	private void decrementIndexesAbove(int key) {
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



	public boolean add(E o) {
		map.put(++size, o);
		return true;
	}


	public void add(int index, E element) {
		incrementIndexes(index);
		map.put(index, element);
	}

	private void incrementIndexes(int index) {
		incrementIndexes(index, 1);
	}
	
	private void incrementIndexes(int index, int increment) {
		List<Integer> indexesToIncrement = new ArrayList<Integer>(map.descendingMap().headMap(index, true).keySet());
		for(int i : indexesToIncrement) {
			map.put(i + increment, map.get(i));
			map.remove(i);
		}
	}
	
	private void decrementIndexes(int index) {
		decrementIndexes(index, 1);
	}
	
	private void decrementIndexes(int index, int decrement) {
		List<Integer> indexesToDecrement = new ArrayList<Integer>(map.tailMap(index, true).keySet());
		for(int i : indexesToDecrement) {
			map.put(i - decrement, map.get(i));
			map.remove(i);
		}
	}

	public boolean addAll(Collection<? extends E> c) {
		boolean result = false;
		for(E o : c) {
			result = result || add(o);
		}
		return result;
	}



	public boolean addAll(int index, Collection<? extends E> c) {
		incrementIndexes(index, c.size());
		int pointer = index;
		boolean result = false;
		for(E o : c) {
			map.put(pointer++, o);
			result = true;
		}
		return result;
	}



	public void clear() {
		map.clear();
		size = 0;
	}



	public boolean contains(Object o) {
		return map.containsValue(o);
	}



	public boolean containsAll(Collection<?> c) {
		return map.values().containsAll(c);
	}



	public E get(int index) {
		return map.get(index);
	}



	public int indexOf(Object o) {
		for(Entry<Integer, E> entry : map.entrySet()) {
			if(entry.getValue().equals(o)) {
				return entry.getKey();
			}
		}
		return -1;
	}



	public boolean isEmpty() {
		return size == 0;
	}



	public Iterator<E> iterator() {
		return map.values().iterator();
	}



	public int lastIndexOf(Object o) {
		for(Entry<Integer, E> entry : map.descendingMap().entrySet()) {
			if(entry.getValue().equals(o)) {
				return entry.getKey();
			}
		}
		return -1;
	}



	public ListIterator<E> listIterator() {
		throw new UnsupportedOperationException();
	}



	public ListIterator<E> listIterator(int index) {
		throw new UnsupportedOperationException();
	}



	public boolean remove(Object o) {
		int index = indexOf(o);
		if(index > -1) {
			remove(index);
			return true;
		}
		return false;
	}



	public E remove(int index) {
		E result = map.remove(index);
		decrementIndexes(index);
		--size;
		return result;
	}



	public boolean removeAll(Collection<?> c) {
		throw new UnsupportedOperationException();
	}



	public boolean retainAll(Collection<?> c) {
		throw new UnsupportedOperationException();
	}



	public E set(int index, E element) {
		return map.put(index, element);
	}



	public int size() {
		return size;
	}



	public List<E> subList(int fromIndex, int toIndex) {
		throw new UnsupportedOperationException();
	}



	public Object[] toArray() {
		throw new UnsupportedOperationException();
	}



	public <T> T[] toArray(T[] a) {
		throw new UnsupportedOperationException();
	}
	
}
