package org.katlas.JavaKh.rows;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import org.katlas.JavaKh.JavaKh;
import org.katlas.JavaKh.LCCCMap;
import org.katlas.JavaKh.SmoothingColumn;
import org.katlas.JavaKh.interfaces.CannedCobordism;
import org.katlas.JavaKh.interfaces.LCCC;

public class LinkedListRow<F> implements MatrixRow<F> {

	/**
	 * 
	 */
	private static final long serialVersionUID = 8815364403706665179L;

	private transient Entry<F> firstEntry;
	
	private transient Entry<F> lastEntry;
	private transient Entry<F> cachedEntry;

	static class Entry<F> {
		int index;
		F value;
		Entry<F> next;

		Entry(int index, F value) {
			this.index = index;
			this.value = value;
			this.next = null;
		}
	}

	public void compact() {
	}

	public boolean containsKey(int key) {
		if (lastEntry != null) {
			if (key > lastEntry.index)
				return false;
			if (key == lastEntry.index) {
				cachedEntry = lastEntry;
				return true;
			}
		}

		Entry<F> entry;

		if (cachedEntry != null) {
			if (key == cachedEntry.index)
				return true;
			else if (key > cachedEntry.index) {
				entry = cachedEntry;
			} else {
				entry = firstEntry;
			}
		} else {
			entry = firstEntry;
		}

		while (entry != null && entry.index <= key) {
			if (entry.index == key) {
				cachedEntry = entry;
				return true;
			}
			entry = entry.next;
		}
		return false;
	}

	public void decrementIndexesAbove(int j) {
		Entry<F> entry = firstEntry;
		while (entry != null) {
			if (entry.index > j) {
				entry.index--;
			}
			entry = entry.next;
		}
	}

	public F get(int key) {
		if (cachedEntry != null && cachedEntry.index == key) {
			return cachedEntry.value;
		}

		// assert false; // hopefully we never pass this point!

		Entry<F> entry = firstEntry;
		while (entry != null && entry.index <= key) {
			if (entry.index == key) {
				cachedEntry = entry;
				return entry.value;
			}
			entry = entry.next;
		}
		return null;
	}

	public Iterable<Integer> keys() {
		return new Iterable<Integer>() {

			public Iterator<Integer> iterator() {
				return new Iterator<Integer>() {

					Entry<F> nextEntry = firstEntry;

					public boolean hasNext() {
						return nextEntry != null;
					}

					public Integer next() {
						int result = nextEntry.index;
						cachedEntry = nextEntry;
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
		if (f == null) {
			remove(key);
			return;
		}

		if (lastEntry != null) {
			if (key > lastEntry.index) {
				lastEntry.next = new Entry<F>(key, f);
				lastEntry = lastEntry.next;
				return;
			} else if (key == lastEntry.index) {
				lastEntry.value = f;
				return;
			}
		}

		Entry<F> entry;

		if (cachedEntry != null) {
			if (cachedEntry.index == key) {
				cachedEntry.value = f;
				return;
			} else if (cachedEntry.index < key) {
				entry = cachedEntry;
				cachedEntry = null;
			} else {
				entry = firstEntry;
				cachedEntry = null;
			}
		} else {
			entry = firstEntry;
		}

		if (entry == null) {
			firstEntry = new Entry<F>(key, f);
			lastEntry = firstEntry;
			return;
		}
		if (entry.index > key) {
			Entry<F> newEntry = new Entry<F>(key, f);
			newEntry.next = entry;
			firstEntry = newEntry;
			return;
		}
		if (entry.index == key) {
			entry.value = f;
			return;
		}

		while (entry.next != null && entry.next.index < key) {
			entry = entry.next;
		}
		if (entry.next == null) {
			if (entry.index == key) {
				entry.value = f;
				lastEntry = entry;
				return;
			} else {
				entry.next = new Entry<F>(key, f);
				lastEntry = entry.next;
				return;
			}
		} else {
			if (entry.next.index == key) {
				entry.next.value = f;
				return;
			} else {
				Entry<F> newEntry = new Entry<F>(key, f);
				newEntry.next = entry.next;
				entry.next = newEntry;
				return;
			}
		}
	}

	public void putLast(int key, F f) {
		if (f == null) {
			remove(key);
			return;
		}

		if (lastEntry != null) {
			if (key > lastEntry.index) {
				lastEntry.next = new Entry<F>(key, f);
				lastEntry = lastEntry.next;
				return;
			} else if (key == lastEntry.index) {
				lastEntry.value = f;
				return;

			} else {
				assert false;
				return;
			}
		} else {
			if(firstEntry == null) {
				firstEntry = new Entry<F>(key, f);
				lastEntry = firstEntry;
				return;
			} else {
				put(key, f);
			}
		}
	}

	public void remove(int j) {
		cachedEntry = null;
		if (lastEntry != null) {
			if (lastEntry.index < j)
				return;
			if (lastEntry.index == j)
				lastEntry = null;
		}

		Entry<F> entry;
		if (cachedEntry != null) {
			if (cachedEntry.index < j) { // can only do strictly less than here.
				entry = cachedEntry;
			} else {
				entry = firstEntry;
			}
		} else {
			entry = firstEntry;
		}

		if (entry == null) {
			return;
		}
		if (entry.index == j) {
			firstEntry = entry.next;
			cachedEntry = entry.next;
			return;
		} else {
			while (entry.next != null && entry.next.index < j) {
				entry = entry.next;
			}
			if (entry.next != null && entry.next.index == j) {
				entry.next = entry.next.next;
				cachedEntry = entry;
			}
		}
	}

	public void clear() {
		firstEntry = null;
		cachedEntry = null;
		lastEntry = null;
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
