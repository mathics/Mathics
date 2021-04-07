package org.katlas.JavaKh.LCCC;

import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Map;
import java.util.Set;
import java.util.SortedMap;
import java.util.TreeMap;

public class SizeRarelyMoreThanOneMap<K, V> implements SortedMap<K, V> {

  private SortedMap<K, V> map;
  private K               firstKey;
  private V               firstValue;

  protected SortedMap<K, V> newMap() {
    return new TreeMap<K, V>();
  }

  public void clear() {
    map = null;
    firstKey = null;
    firstValue = null;
  }

  public boolean containsKey(Object key) {
    return (firstKey != null && firstKey.equals(key)) || (map != null && map.containsKey(key));
  }

  public boolean containsValue(Object value) {
    return (firstValue != null && firstValue.equals(value)) || (map != null && map.containsValue(value));
  }

  public V get(Object key) {
    if (firstKey != null && firstKey.equals(key)) {
      return firstValue;
    } else if (map != null) {
      return map.get(key);
    } else {
      return null;
    }
  }

  public boolean isEmpty() {
    return firstKey == null && (map == null || map.isEmpty());
  }

  public V put(K key, V value) {
    if (value == null) {
      return remove(key);
    }

    if (firstKey == null) {
      if (map == null) {
        firstKey = key;
        firstValue = value;
        return null;
      } else {
        return map.put(key, value);
      }
      // unreachable
    } else {
      if (firstKey.equals(key)) {
        V result = firstValue;
        firstValue = value;
        return result;
      } else {
        // move to the map
        assert map == null;
        map = newMap();
        map.put(firstKey, firstValue);
        firstKey = null;
        firstValue = null;
        return map.put(key, value);
      }
      // unreachable
    }
  }

  public void putAll(Map<? extends K, ? extends V> otherMap) {
    if (otherMap.size() > 1) {
      // switch to the map
      if (map == null) {
        map = newMap();
        if (firstKey != null) {
          map.put(firstKey, firstValue);
          firstKey = null;
          firstValue = null;
        }
      }
      map.putAll(otherMap);
      return;
    } else {
      for (Map.Entry<? extends K, ? extends V> entry : otherMap.entrySet()) {
        put(entry.getKey(), entry.getValue());
      }
      return;
    }
  }

  public V remove(Object key) {
    if (firstKey != null && firstKey.equals(key)) {
      firstKey = null;
      V result = firstValue;
      firstValue = null;
      return result;
    } else if (map != null) {
      V result = map.remove(key);
      if (map.isEmpty()) {
        map = null;
      } else {
        if (map.size() == 1) {
          for (Map.Entry<K, V> entry : map.entrySet()) {
            firstKey = entry.getKey();
            firstValue = entry.getValue();
          }
          map = null;
        }
      }
      return result;
    } else {
      return null;
    }
  }

  public int size() {
    return (firstKey == null ? (map == null ? 0 : map.size()) : 1);
  }

  public Set<K> keySet() {
    if (firstKey == null) {
      if (map == null) {
        return Collections.emptySet();
      } else {
        return map.keySet();
      }
    } else {
      assert map == null;
      return Collections.singleton(firstKey);
    }
  }

  public Set<Map.Entry<K, V>> entrySet() {
    assert false; // this code sucks, hope that it never actually runs.

    if (firstKey == null) {
      if (map == null) {
        return Collections.emptySet();
      } else {
        return map.entrySet();
      }
    } else {
      assert map == null;
      Entry<K, V> entry = new Map.Entry<K, V>() {

        public K getKey() {
          return firstKey;
        }

        public V getValue() {
          return firstValue;
        }

        public V setValue(V value) {
          V result = firstValue;
          firstValue = value;
          return result;
        }

      };
      return Collections.singleton(entry);
    }
  }

  public Collection<V> values() {
    if (firstKey == null) {
      if (map == null) {
        return Collections.emptySet();
      } else {
        return map.values();
      }
    } else {
      assert map == null;
      return Collections.singleton(firstValue);
    }
  }

  public Comparator<? super K> comparator() {
    return null;
  }

  public K firstKey() {
    return (firstKey == null) ? ((map == null) ? null : map.firstKey()) : firstKey;
  }

  @SuppressWarnings("unchecked")
  public SortedMap<K, V> headMap(K key) {
    if (map == null) {
      if (firstKey == null) {
        return emptySortedMap();
      } else {
        if (key instanceof Comparable) {
          if (((Comparable) key).compareTo(firstKey) <= 0) {
            return emptySortedMap();
          } else {
            return this;
          }
        } else {
          throw new ClassCastException();
        }
      }
    } else {
      return map.headMap(key);
    }
  }

  public K lastKey() {
    if (map == null) {
      return firstKey;
    } else {
      return map.lastKey();
    }
  }

  @SuppressWarnings("unchecked")
  public SortedMap<K, V> subMap(K key1, K key2) {
    if (map == null) {
      if (firstKey == null) {
        return emptySortedMap();
      } else {
        if (key1 instanceof Comparable && key2 instanceof Comparable) {
          if (((Comparable) key1).compareTo(firstKey) > 0 || ((Comparable) key2).compareTo(firstKey) <= 0) {
            return emptySortedMap();
          } else {
            return this;
          }
        } else {
          throw new ClassCastException();
        }
      }
    } else {
      return map.subMap(key1, key2);
    }
   }

  @SuppressWarnings("unchecked")
  public SortedMap<K, V> tailMap(K key) {
    if (map == null) {
      if (firstKey == null) {
        return emptySortedMap();
      } else {
        if (key instanceof Comparable) {
          if (((Comparable) key).compareTo(firstKey) > 0) {
            return emptySortedMap();
          } else {
            return this;
          }
        } else {
          throw new ClassCastException();
        }
      }
    } else {
      return map.tailMap(key);
    }
  }

  @SuppressWarnings("unchecked")
  private static SortedMap EMPTY_SORTED_MAP;
  
  @SuppressWarnings("unchecked")
  private static <K, V> SortedMap<K, V> emptySortedMap() {
    if(EMPTY_SORTED_MAP == null) {
      EMPTY_SORTED_MAP = new TreeMap();
    }
    return (SortedMap<K, V>)EMPTY_SORTED_MAP;
  }
  
}
