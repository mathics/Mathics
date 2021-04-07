package org.katlas.JavaKh.utils;
import java.io.Serializable;
import java.util.Collection;
import java.util.Map;
import java.util.Set;

/**
 * 
 */

public class AlwaysEmptyMap<K,V> implements Map<K,V>, Serializable {

	/**
	 * 
	 */
	private static final long serialVersionUID = 2680627961266068327L;

	public void clear() {
		return;
	}

	public boolean containsKey(Object key) {
		return false;
	}

	public boolean containsValue(Object value) {
		return false;
	}

	public Set<Entry<K, V>> entrySet() {
		return new AlwaysEmptySet<Entry<K,V>>();
	}

	public V get(Object key) {
		return null;
	}

	public boolean isEmpty() {
		return true;
	}

	public Set<K> keySet() {
		return new AlwaysEmptySet<K>();
	}

	public V put(K key, V value) {
		return null;
	}

	public void putAll(Map<? extends K, ? extends V> m) {
		
	}

	public V remove(Object key) {
		return null;
	}

	public int size() {
		return 0;
	}

	public Collection<V> values() {
		return new AlwaysEmptySet<V>();
	}
	
}