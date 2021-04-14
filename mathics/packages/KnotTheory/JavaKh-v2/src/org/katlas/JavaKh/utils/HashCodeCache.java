package org.katlas.JavaKh.utils;

import gnu.trove.THashSet;
import gnu.trove.TObjectHashingStrategy;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

public class HashCodeCache<E> extends THashSet<E> implements Cache<E> {

  private static final Log  log    = LogFactory.getLog(HashCodeCache.class);

  private transient long    hits   = 0;
  private transient long    checks = 0;
  
  public HashCodeCache() {
    super();
  }
  
  public HashCodeCache(TObjectHashingStrategy<E> strategy) {
    super(strategy);
  }
  
  @SuppressWarnings("unchecked")
  public synchronized E cache(E e) {
    ++checks;
    
    int i = index(e);
    if(i == -1) {
      add(e);
      return e;
    } else {
      E result = (E) _set[i];
      if(e != result && !_hashingStrategy.equals(e, result)) {
        if (_hashingStrategy.computeHashCode(result) != _hashingStrategy.computeHashCode(e)) {
          log.info("Hashcode has mysteriously changed?");
        }
        // hash collision...
      } else {
        ++hits;
        return result;
      }
      add(e);
      return e;
    }
  }

  public long getNumberOfChecks() {
    return checks;
  }

  public long getNumberOfHits() {
    return hits;
  }

}
