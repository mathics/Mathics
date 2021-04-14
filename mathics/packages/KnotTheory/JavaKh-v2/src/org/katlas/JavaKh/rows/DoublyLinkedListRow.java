package org.katlas.JavaKh.rows;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.util.Iterator;

public class DoublyLinkedListRow<F> implements MatrixRow<F> {

	/**
	 * 
	 */
	private static final long serialVersionUID = -2450756606041401437L;
	private transient Entry first, last, current;

	class Entry {
		int index;
		F value;
		Entry previous, next;

		Entry(int index, F value) {
			this.index = index;
			this.value = value;
			this.next = null;
		}
	}

	public void compact() {
	}

	public boolean containsKey(int key) {
		return get(key) != null;
	}

	public void decrementIndexesAbove(int j) {
		if(current == null) return;
		Entry entry;
		if (!containsKey(j)) {
			if (current.index < j) {
				entry = current.next;
			} else {
				entry = current;
			}
		} else {
			// this shouldn't happen, recover anyway.
			assert false;
			entry = current;
		}
		while (entry != null) {
			--(entry.index);
			entry = entry.next;
		}
	}

	public F get(int key) {
		if (current == null)
			return null;
		
		if (key == current.index)
			return current.value;

		if (key < first.index) {
			current = first;
			return null;
		}
		if (key == first.index) {
			current = first;
			return current.value;
		}

		if (key > last.index) {
			current = last;
			return null;
		}
		if (key == last.index) {
			current = last;
			return current.value;
		}

		if (key > current.index) {
			while (true) {
				if (current.next == null) {
					return null;
				} else {
					if (current.next.index < key) {
						current = current.next;
					} else if (current.next.index == key) {
						current = current.next;
						return current.value;
					} else {
						return null;
					}
				}
			}
		} else {
			while (true) {
				if (current.previous == null) {
					return null;
				} else {
					if (current.previous.index > key) {
						current = current.previous;
					} else if (current.previous.index == key) {
						current = current.previous;
						return current.value;
					} else {
						return null;
					}
				}
			}
		}
	}

	public Iterable<Integer> keys() {
		return new Iterable<Integer>() {

			public Iterator<Integer> iterator() {
				return new Iterator<Integer>() {

					Entry nextEntry = first;

					public boolean hasNext() {
						return nextEntry != null;
					}

					public Integer next() {
						int result = nextEntry.index;
						current = nextEntry;
						nextEntry = nextEntry.next;
						return result;
					}

					public void remove() {
						throw new UnsupportedOperationException();
					}

				};
			}

		};
	}

	public void put(int key, F f) {
		if (containsKey(key)) {
			current.value = f;
		} else {
			Entry newEntry = new Entry(key, f);
			if (current == null) {
				current = last = first = newEntry;
			} else {
				// we're next to it!
				if (current.index < key) {
					newEntry.previous = current;
					newEntry.next = current.next;
					if (current.next != null) {
						current.next.previous = newEntry;
					} else {
						last = newEntry;
					}
					current.next = newEntry;
					current = newEntry;
				} else {
					newEntry.next = current;
					newEntry.previous = current.previous;
					if (current.previous != null) {
						current.previous.next = newEntry;
					} else {
						first = newEntry;
					}
					current.previous = newEntry;
					current = newEntry;
				}
			}
		}
	}

	public void putLast(int key, F f) {
		if(last == null) {
			current = last = first = new Entry(key, f);
			return;
		}
		
		if (last.index == key) {
			last.value = f;
			current = last;
			return;
		} else {
			assert last.index < key;
			Entry newEntry = new Entry(key, f);
			last.next = newEntry;
			newEntry.previous = last;
			current = last = newEntry;
		}
	}

	public void remove(int j) {
		if (containsKey(j)) {
			if (current.next != null) {
				current.next.previous = current.previous;
			} else {
				last = current.previous;
			}
			if (current.previous != null) {
				current.previous.next = current.next;
				current = current.previous;
			} else {
				first = current.next;
				current = current.next;
			}
		}
	}

	public void clear() {
		first = last = current = null;
	}

	private void writeObject(ObjectOutputStream s) throws IOException {
		for(int i : keys()) {
			s.writeInt(i);
			s.writeObject(get(i));
		}
		s.writeInt(-1);
	}

	@SuppressWarnings("unchecked")
	private void readObject(ObjectInputStream s) throws IOException,
			ClassNotFoundException {
		int key;
		while((key = s.readInt()) != -1) {
			putLast(key, (F) s.readObject());
		}
	}
	
}
