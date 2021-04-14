package org.katlas.JavaKh.utils;

import java.util.Collection;
import java.util.Map;
import java.util.Set;

public class NonNullValueMapWrapper<K, V> implements Map<K, V> {

	private final Map<K, V> internalMap;

	public NonNullValueMapWrapper(Map<K, V> internalMap) {
		for(V v : internalMap.values()) {
			if(v == null) {
				throw new IllegalArgumentException("Cannot wrap a map which already contains null values.");
			}
		}
		this.internalMap = internalMap;
	}
	
	public void clear() {
		internalMap.clear();
	}

	public boolean containsKey(Object key) {
		return internalMap.containsKey(key);
	}

	public boolean containsValue(Object value) {
		return internalMap.containsValue(value);
	}

	public Set<java.util.Map.Entry<K, V>> entrySet() {
		return internalMap.entrySet();
	}

	public V get(Object key) {
		return internalMap.get(key);
	}

	public boolean isEmpty() {
		return internalMap.isEmpty();
	}

	public Set<K> keySet() {
		return internalMap.keySet();
	}

	public V put(K key, V value) {
		if(value == null) {
			throw new IllegalArgumentException("Tried to add a null value.");
		}
		return internalMap.put(key, value);
	}

	public void putAll(Map<? extends K, ? extends V> m) {
		for(K k : m.keySet()) {
			put(k, m.get(k));
		}
	}

	public V remove(Object key) {
		return internalMap.remove(key);
	}

	public int size() {
		return internalMap.size();
	}

	public Collection<V> values() {
		return internalMap.values();
	}

}
