package org.katlas.JavaKh;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.katlas.JavaKh.LCCC.SizeRarelyMoreThanOneMap;
import org.katlas.JavaKh.algebra.Ring;
import org.katlas.JavaKh.algebra.implementations.LinearComboMap;
import org.katlas.JavaKh.algebra.rings.Rings;
import org.katlas.JavaKh.interfaces.CannedCobordism;
import org.katlas.JavaKh.interfaces.LCCC;

public class LCCCMap<R extends Ring<R>> extends LinearComboMap<R, Cap, CannedCobordism, LCCC<R>> implements LCCC<R>,
    Serializable {
  private static final long serialVersionUID = 8539035436108747574L;
	private static final Log log = LogFactory.getLog(LCCCMap.class);

  transient boolean         alreadyReduced   = false;

  public LCCCMap() {
    super();
  }

  public LCCCMap(Map<CannedCobordism, R> terms) {
    super();
    coefficients.putAll(terms);
  }

  public LCCCMap(CannedCobordism cc, R r) {
    super(cc, r);
  }

  @Override
  protected Map<CannedCobordism, R> newMap() {
    /* 
     * Performance data from ms3, at rev ~476. (with -G, intense garbage collection)
     * 
     * T(8,7)
     * 
     * SizeRarelyMoreThanOneMap
     * INFO - Peak memory usage: 950,488,128
     * INFO - Elapsed time: 3,764,186
    
     * TreeMap
     * INFO - Peak memory usage: 1,007,876,128
     * INFO - Elapsed time: 3,646,449
     * 
     * 
     * T(7,6)
     * 
     * 	SizeRarelyMoreThanOneMap
     * 		INFO - Peak memory usage: 153,852,960
     * 		INFO - Elapsed time: 402,758
     * 
     * 	  	(without -G):
     * 		INFO - Peak memory usage: 742,332,616
     * 		INFO - Elapsed time: 18,581
     * 
     *    (and after byte array caching...)
     *    INFO - Peak memory usage: 174,748,960
     *    INFO - Elapsed time: 379,354
     *    
     *      (without -G):
     *    INFO - Peak memory usage: 772,997,560
     *    INFO - Elapsed time: 23,532
     *    
     *    (and after the real optimisation!)    
     *    INFO - Peak memory usage: 14,489,352
     *    INFO - Elapsed time: 277,047
     *
     *      
     * 	TreeMap
     * 		INFO - Peak memory usage: 131,218,096
     * 		INFO - Elapsed time: 403,006
     * 
     *    (and after the real optimisation!) The custom implementation wins!
     *    INFO - Peak memory usage: 15,995,800
     *    INFO - Elapsed time: 296,166
     *    
     */
    //    return new TreeMap<CannedCobordism, R>();
    //    return new ListMap<CannedCobordism, R>();
    return new SizeRarelyMoreThanOneMap<CannedCobordism, R>();
  }

  @SuppressWarnings("unchecked")
  public boolean equals(Object o) {
    if (o == null && numberOfTerms() == 0)
      return true;
    if (!(o instanceof LCCC))
      return false;
    LCCC<R> other = (LCCC<R>) o;
    if (other.numberOfTerms() != numberOfTerms())
      return false;
    for (CannedCobordism term : terms()) {
      if (!getCoefficient(term).equals(other.getCoefficient(term)))
        return false;
    }
    return true;
  }

  public LCCC<R> compose(int start, CannedCobordism cc, int cstart, int nc, boolean reverse) {

    if (numberOfTerms() == 0) {
      assert false;
      return null;
    }

    LCCC<R> ret = null;

    CannedCobordism composition;

    if (reverse) {
      for (CannedCobordism occ : coefficients.keySet()) {
        composition = cc.compose(cstart, occ, start, nc);
        if (ret == null) {
          ret = new LCCCMap<R>();
        }
        ret = ret.add(composition, coefficients.get(occ));
      }
    } else {
      for (CannedCobordism occ : coefficients.keySet()) {
        composition = occ.compose(start, cc, cstart, nc);
        if (ret == null) {
          ret = new LCCCMap<R>();
        }
        ret = ret.add(composition, coefficients.get(occ));
      }
    }

    return ret == null ? null : ret.compact();
  }

  @Override
  public LCCC<R> add(CannedCobordism m, int num) {
    alreadyReduced = false;
    return super.add(m, num);
  }

  @Override
  public LCCC<R> add(CannedCobordism cc, R num) {
    alreadyReduced = false;
    return super.add(cc, num);
  }

  @Override
  public LCCC<R> add(LCCC<R> other) {
    alreadyReduced = false;
    return super.add(other);
  }

  @Override
  public LCCC<R> compose(LCCC<R> other) {
    alreadyReduced = false;
    return super.compose(other);
  }

  public LCCC<R> reduce() {

    if (JavaKh.using_h)
      return reduceWithH();
    if (numberOfTerms() == 0)
      return compact();

    if (alreadyReduced) {
      return this;
    }

    LCCC<R> ret = new LCCCMap<R>();
    for (CannedCobordism icc : coefficients.keySet()) {

      if (!(icc instanceof CannedCobordismImpl)) {
        throw new UnsupportedOperationException();
      }
      CannedCobordismImpl cc = (CannedCobordismImpl) icc;

      R num = coefficients.get(cc);
      cc.reverseMaps();
      byte dots[] = new byte[cc.nbc];
      byte genus[] = CannedCobordismImpl.zeros[cc.nbc];
      int moreWork[] = new int[cc.ncc];
      int nmoreWork = 0;
      boolean kill = false;
      for (int i = 0; i < cc.ncc; i++)
        if (cc.genus[i] + cc.dots[i] > 1) {
          kill = true;
        } else if (cc.boundaryComponents[i].length == 0) {
          if (cc.genus[i] == 1)
            num = num.multiply(2);
          else if (cc.dots[i] == 0)
            kill = true;
        } else if (cc.boundaryComponents[i].length == 1) {
          dots[cc.boundaryComponents[i][0]] = (byte) (cc.dots[i] + cc.genus[i]);
          if (cc.genus[i] == 1)
            num = num.multiply(2);
        } else {
          // neck cutting relation
          if (cc.genus[i] + cc.dots[i] == 1) { // only one choice
            if (cc.genus[i] == 1) {
              // there is a 2 from the torus
              num = num.multiply(2);
            }
            // use dots to cancel out the other factors
            for (int j = 0; j < cc.boundaryComponents[i].length; j++)
              dots[cc.boundaryComponents[i][j]] = 1;
          } else {
            // cc.bC[i].length choices
            // use dots to cancel out all the factors, except
            // one which is cancelled by the torus
            // do these last
            moreWork[nmoreWork++] = i;
          }
        }
      if (kill)
        continue;
      byte neckCutting[][] = new byte[1][];
      neckCutting[0] = dots;
      for (int i = 0; i < nmoreWork; i++) {
        int concomp = moreWork[i];
        int nbc = cc.boundaryComponents[concomp].length;
        byte newarr[][] = new byte[neckCutting.length * nbc][cc.nbc];
        for (int j = 0; j < neckCutting.length; j++) {
          System.arraycopy(neckCutting[j], 0, newarr[j * nbc], 0, cc.nbc);
          for (int k = 0; k < nbc; k++)
            newarr[j * nbc][cc.boundaryComponents[concomp][k]] = 1;
          for (int k = 1; k < nbc; k++)
            System.arraycopy(newarr[j * nbc], 0, newarr[j * nbc + k], 0, cc.nbc);
          for (int k = 0; k < nbc; k++)
            newarr[j * nbc + k][cc.boundaryComponents[concomp][k]] = 0;
        }
        neckCutting = newarr;
      }
      byte connectedComponent[] = CannedCobordismImpl.counting[cc.nbc];
      for (int i = 0; i < neckCutting.length; i++) {
        CannedCobordismImpl newcc = new CannedCobordismImpl(source(), target());
        // IMPORTANT!!! in order for them to safely share arrays
        // CannedCobordisms must be treated as immutable
        newcc.connectedComponent = connectedComponent;
        newcc.ncc = newcc.nbc;
        newcc.genus = genus;
        newcc.dots = neckCutting[i];
        ret = ret.add(CannedCobordismImpl.cobordismCache.cache(newcc), num);
      }
    }

    ret = ret == null ? null : ret.compact();
    if (ret instanceof LCCCMap) {
      ((LCCCMap<R>) ret).alreadyReduced = true;
    }
    //		else if (ret instanceof SingleTermLCCC) {
    //			((SingleTermLCCC<R>) ret).alreadyReduced = true;
    //		}
    return ret;
  }

  public LCCC<R> reduceWithH() {
    if (numberOfTerms() == 0)
      return compact();

    if (alreadyReduced) {
      return this;
    }

    Rings<R> ring = Rings.current();

    LCCC<R> ret = new LCCCMap<R>();
    for (CannedCobordism icc : terms()) {

      if (!(icc instanceof CannedCobordismImpl)) {
        throw new UnsupportedOperationException();
      }
      CannedCobordismImpl cc = (CannedCobordismImpl) icc;

      R num = coefficients.get(cc);
      cc.reverseMaps();
      byte dots[] = new byte[cc.nbc];
      int hpow = cc.hpower;
      int moreWork[] = new int[cc.ncc];
      int nmoreWork = 0;
      boolean kill = false;
      for (int i = 0; i < cc.ncc; i++)
        if (cc.boundaryComponents[i].length == 0) {
          if (cc.dots[i] > 0)
            hpow += cc.dots[i] + cc.genus[i] - 1;
          else if (cc.genus[i] % 2 == 0)
            kill = true;
          else {
            num = num.multiply(2);
            hpow += cc.genus[i] - 1;
          }
        } else if (cc.boundaryComponents[i].length == 1) {
          if (cc.dots[i] > 0) {
            hpow += cc.dots[i] + cc.genus[i] - 1;
            dots[cc.boundaryComponents[i][0]] = 1;
          } else if (cc.genus[i] % 2 == 0)
            hpow += cc.genus[i];
          else
            moreWork[nmoreWork++] = i;
        } else {
          if (cc.dots[i] > 0) {
            // the dot and the -h terms cancel since dot == h
            for (int j = 0; j < cc.boundaryComponents[i].length; j++)
              dots[cc.boundaryComponents[i][j]] = 1;
            hpow += cc.dots[i] + cc.genus[i] - 1;
          } else
            moreWork[nmoreWork++] = i;
        }
      if (kill)
        continue;
      byte nCdots[][] = new byte[1][];
      int nChpow[] = new int[1];
      List<R> nCnum = new ArrayList<R>(1);
      nCnum.add(num);
      nCdots[0] = dots;
      nChpow[0] = hpow;
      for (int i = 0; i < nmoreWork; i++) {
        int concomp = moreWork[i];
        int nbc = cc.boundaryComponents[concomp].length;
        assert cc.dots[concomp] == 0;
        byte newdots[][] = new byte[nCdots.length << nbc][cc.nbc];
        int newhpow[] = new int[nChpow.length << nbc];
        List<R> newnum = new ArrayList<R>(nCnum.size() << nbc);
        for (int s = 0; s < nCnum.size() << nbc; ++s) {
          newnum.add(ring.ZERO);
        }
        assert newnum.size() == nCnum.size() << nbc;
        for (int j = 0; j < nCdots.length; j++) {
          for (int k = 0; k < (1 << nbc); k++) {
            int idx = (j << nbc) + k;
            System.arraycopy(nCdots[j], 0, newdots[idx], 0, cc.nbc);
            newhpow[idx] = nChpow[j];
            newnum.set(idx, nCnum.get(j));
            int nzeros = 0;
            for (int l = 0; l < nbc; l++)
              if ((k & (1 << l)) == 0) {
                newdots[idx][cc.boundaryComponents[concomp][l]] = 0;
                nzeros++;
              } else
                newdots[idx][cc.boundaryComponents[concomp][l]] = 1;
            R nmul = ring.ZERO;
            int hmod = 0;
            boolean hset = false;
            for (int l = 0; l < (1 << nzeros); l++) {
              int ndots = 0;
              for (int m = 0; m < nzeros; m++)
                if ((l & (1 << m)) == 0)
                  ndots++;
              if (ndots > 0) {
                if (hset) {
                  if (hmod != nzeros + cc.genus[concomp] - 1)
                    throw new AssertionError();
                } else {
                  hmod = nzeros + cc.genus[concomp] - 1;
                  hset = true;
                }
                int n = 1;
                for (int o = 0; o < nzeros - ndots; o++)
                  n = -n;
                nmul = nmul.add(ring.createInstance(n));
              } else if (cc.genus[concomp] % 2 == 0)
                continue; // coefficient of zero
              else {
                if (hset) {
                  if (hmod != nzeros + cc.genus[concomp] - 1)
                    throw new AssertionError();
                } else {
                  hmod = nzeros + cc.genus[concomp] - 1;
                  hset = true;
                }
                int n = 2;
                for (int o = 0; o < nzeros; o++)
                  n = -n;
                nmul = nmul.add(ring.createInstance(n));
              }
            }
            newhpow[idx] += hmod;
            newnum.set(idx, newnum.get(idx).multiply(nmul));
          }
        }
        nCdots = newdots;
        nChpow = newhpow;
        nCnum = newnum;
      }
      for (int i = 0; i < nCdots.length; i++) {
        CannedCobordismImpl newcc = new CannedCobordismImpl(source(), target());
        newcc.connectedComponent = CannedCobordismImpl.counting[newcc.nbc];
        newcc.ncc = newcc.nbc;
        newcc.genus = CannedCobordismImpl.zeros[cc.nbc];
        newcc.dots = nCdots[i];
        newcc.hpower = nChpow[i];
        ret = ret.add(CannedCobordismImpl.cobordismCache.cache(newcc), nCnum.get(i));
      }
    }

    ret = ret.compact();
    if (ret instanceof LCCCMap) {
      ((LCCCMap<R>) ret).alreadyReduced = true;
    }
    //		else if (ret instanceof SingleTermLCCC) {
    //			((SingleTermLCCC<R>) ret).alreadyReduced = true;
    //		}
    return ret;
  }

  public LCCC<R> finalizeH() {
    if (numberOfTerms() == 0)
      return null;
    assert source().n == 2 && source().ncycles == 0 && target().n == 2 && target().ncycles == 0;
    LCCC<R> ret = new LCCCMap<R>();
    CannedCobordismImpl cc = (CannedCobordismImpl) CannedCobordismImpl.isomorphism(source());
    boolean hset = false;
    for (CannedCobordism iocc : coefficients.keySet()) {
      if (!(iocc instanceof CannedCobordismImpl)) {
        throw new UnsupportedOperationException();
      }
      CannedCobordismImpl occ = (CannedCobordismImpl) iocc;

      if (!coefficients.get(occ).isZero()) {
        if (!hset)
          cc.hpower = occ.hpower + occ.dots[0] + occ.genus[0];
        else if (cc.hpower != occ.hpower + occ.dots[0] + occ.genus[0])
          throw new AssertionError();
        ret = ret.add(cc, coefficients.get(occ));
      }
    }
    return ret.compact();
  }

  @Override
  public LCCC<R> flexibleZeroLinearCombo() {
    return new LCCCMap<R>();
  }

  @Override
  public LCCC<R> fixedZeroLinearCombo() {
    return new ZeroLCCC<R>();
  }

  @Override
  public LCCC<R> singleTermLinearCombo(CannedCobordism mor, R r) {
    /*
     * WARNING
     * SingleTermLCCC is somehow horribly broken. Don't use it.
     * You've been warned.
     */
    //		return new SingleTermLCCC<R>(mor, r);
    LCCCMap<R> result = new LCCCMap<R>(mor, r);
    return result;
  }

  public LCCC<R> compact() {
    if (numberOfTerms() == 0)
      return fixedZeroLinearCombo();
    else
      return this;
  }

	@SuppressWarnings("unchecked")
	private void readObject(ObjectInputStream s) throws IOException,
	ClassNotFoundException {
		int serializationVersion = s.readInt();
		if (serializationVersion == 1) {
			int numberOfTerms = s.readInt();
			if (numberOfTerms > 0) {
				Map<CannedCobordism, R> terms = new HashMap<CannedCobordism, R>(
						numberOfTerms);
				for (int k = 0; k < numberOfTerms; ++k) {
					R coefficient = (R) s.readObject();
					CannedCobordism cc = (CannedCobordism) s
							.readObject();
					terms.put(cc, coefficient);
				}
				coefficients = terms;
			}
		} else {
			log.warn("Serialization version looks wrong...");
			assert false;
		}
	}
  
  private void writeObject(ObjectOutputStream s) throws IOException {
    s.writeInt(1); // serialization version
    s.writeInt(numberOfTerms());
    for(CannedCobordism cc : terms()) {
    	s.writeObject(getCoefficient(cc));
    	s.writeObject(cc);
    }
  }
}
