package org.katlas.JavaKh.LCCC;

import java.util.AbstractSet;
import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class ListMap<K, V> implements Map<K, V> {

	private List<K> keys;
	private List<V> values;

	public ListMap() {
    this.keys = newList();
    this.values = newList();
  }

  protected <R> List<R> newList() {
		return new LinkedList<R>();
	}

	public void clear() {
		keys.clear();
		values.clear();
	}

	public boolean containsKey(Object key) {
		return keys.contains(key);
	}

	public boolean containsValue(Object value) {
		return values.contains(value);
	}

	public Set<java.util.Map.Entry<K, V>> entrySet() {
		assert false; // this sucks, hope it never gets called

		return new AbstractSet<Entry<K, V>>() {

			@Override
			public Iterator<java.util.Map.Entry<K, V>> iterator() {
				return new Iterator<Entry<K, V>>() {
					int index = 0;

					public boolean hasNext() {
						return index < keys.size();
					}

					public java.util.Map.Entry<K, V> next() {
						return new java.util.Map.Entry<K, V>() {

							public K getKey() {
								return keys.get(index);
							}

							public V getValue() {
								return values.get(index);
							}

							public V setValue(V value) {
								return values.set(index, value);
							}

						};
					}

					public void remove() {
						throw new UnsupportedOperationException();
					}
				};
			}

			@Override
			public int size() {
				return keys.size();
			}

		};
	}

	public V get(Object key) {
		int index = keys.indexOf(key);
		if (index == -1) {
			return null;
		} else {
			return values.get(index);
		}
	}

	public boolean isEmpty() {
		return keys.isEmpty();
	}

	public Set<K> keySet() {
		return new AbstractSet<K>() {

			@Override
			public Iterator<K> iterator() {
				return keys.iterator();
			}

			@Override
			public int size() {
				return keys.size();
			}

		};
	}

	public V put(K key, V value) {
		int index = keys.indexOf(key);
		if (index == -1) {
			keys.add(key);
			values.add(value);
			return null;
		} else {
			V result = values.get(index);
			values.set(index, value);
			return result;
		}
	}

	public void putAll(Map<? extends K, ? extends V> m) {
		for (Map.Entry<? extends K, ? extends V> entry : m.entrySet()) {
			put(entry.getKey(), entry.getValue());
		}
	}

	public V remove(Object key) {
		int index = keys.indexOf(key);
		if (index == -1) {
			return null;
		} else {
			keys.remove(index);
			return values.remove(index);
		}
	}

	public int size() {
		return keys.size();
	}

	public Collection<V> values() {
		return values;
	}

}
