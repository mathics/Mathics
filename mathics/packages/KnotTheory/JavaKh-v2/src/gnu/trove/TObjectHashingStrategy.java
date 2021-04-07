///////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2002, Eric D. Friedman All Rights Reserved.
//
// This library is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
///////////////////////////////////////////////////////////////////////////////


package gnu.trove;

import java.io.Serializable;

/**
 * Interface to support pluggable hashing strategies in maps and sets.
 * Implementors can use this interface to make the trove hashing
 * algorithms use object values, values provided by the java runtime,
 * or a custom strategy when computing hashcodes.
 * 
 * Created: Sat Aug 17 10:52:32 2002
 *
 * @author Eric Friedman
 * @version $Id: TObjectHashingStrategy.java,v 1.3 2007/06/11 15:26:44 robeden Exp $
 */

public interface TObjectHashingStrategy<T> extends Serializable {

  /**
   * Computes a hash code for the specified object.  Implementors
   * can use the object's own <tt>hashCode</tt> method, the Java
   * runtime's <tt>identityHashCode</tt>, or a custom scheme.
   *
   * @param object for which the hashcode is to be computed
   * @return the hashCode
   */
  int computeHashCode(T object);

  /**
   * Compares o1 and o2 for equality.  Strategy implementors may use
   * the objects' own equals() methods, compare object references,
   * or implement some custom scheme.
   *
   * @param o1 an <code>Object</code> value
   * @param o2 an <code>Object</code> value
   * @return true if the objects are equal according to this strategy.
   */
  boolean equals(T o1, T o2);
} // TObjectHashingStrategy
