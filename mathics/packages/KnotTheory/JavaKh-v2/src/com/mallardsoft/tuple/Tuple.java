package com.mallardsoft.tuple;

/**
 * Ordered set of elements.
 * <p>
 * You can use a tuple as a multi-valued key for hash-based or
 * ordered collections. Tuples properly implement hashCode and
 * equals to compare all items in the tuple with respect to order.
 * As a key to an ordered collection, the tuple gives the first
 * element precedence.
 * <p>
 * You can also use a tuple to return multiple values from a function.
 * Use one of the shorthand types ({@link Pair}, {@link Triple}, etc.)
 * as the return type for the function. Use one of the overloaded
 * {@link Tuple#from(Object,Object)} methods to
 * generate the tuple within the function. Use the {@link Tuple#extract(Variable)} method
 * to fetch values from the tuple back in the caller.
 *
 * @author Michael L Perry
 *
 * @param <First> The type of the first element in the tuple.
 * @param <Rest> The type of the tuple containing the rest of the elements.
 */
public class Tuple<First, Rest> implements SeparatedAppender, Comparable<Tuple<First, Rest>> {

	private First first;
	private Rest rest;

	protected Tuple(First first, Rest rest) {
		this.first = first;
		this.rest = rest;
	}

	/**
	 * Remove the first element from the tuple and return the rest.
	 * To extract all elements from the tuple, chain extract calls.
	 *
	 * @param m
	 * @return
	 */
	public Rest extract(Variable<First> m) {
		m.set(first);
		return rest;
	}

	public <T> Tuple<T, Tuple<First, Rest>> prepend(T m) {
		return new Tuple<T, Tuple<First, Rest>>(m, this);
	}

    // Compare two tuples. All elements must be equal.
    @SuppressWarnings("unchecked")
	public boolean equals(Object obj) {
        if (obj == null)
            return false;
        if (!(obj instanceof Tuple))
            return false;
        Tuple<First, Rest> that = (Tuple<First, Rest>) obj;
        return
        	(this.first == null ? that.first == null : this.first.equals(that.first)) &&
        	this.rest.equals(that.rest);
    }

    // Calculate a hash code based on the hash of each element.
    public int hashCode() {
    	return (first == null ? 0 : first.hashCode()) + rest.hashCode() * 37;
    }

    public String toString() {
    	return toString("(", ", ", ")");
    }

    // Display the tuple using the open, separator, and close.
    public String toString(String open, String separator, String close) {
        StringBuffer result = new StringBuffer();
        result.append(open).append(first);
    	((SeparatedAppender)rest).appendString(result, separator);
        return result.append(close).toString();
    }

    public void appendString(StringBuffer buffer, String separator) {
    	buffer.append(separator).append(first);
    	((SeparatedAppender)rest).appendString(buffer, separator);
	}

	// Order by the most significant element first.
    // The tuples must agree in size and type.
	@SuppressWarnings("unchecked")
	public int compareTo(Tuple<First, Rest> that) {
        int compare = ((Comparable)this.first).compareTo(that.first);
        if (compare != 0)
        	return compare;
        else
        	return ((Comparable)this.rest).compareTo(that.rest);
    }

    public static <T1> Single<T1> from(T1 m1) {
    	return new Single<T1>(m1);
    }

	public static <T1, T2> Pair<T1, T2> from(T1 m1, T2 m2) {
		return new Pair<T1, T2>(m1, m2);
	}

	public static <T1, T2, T3> Triple<T1, T2, T3> from(T1 m1, T2 m2, T3 m3) {
		return new Triple<T1, T2, T3>(m1, m2, m3);
	}

	public static <T1, T2, T3, T4> Quadruple<T1, T2, T3, T4> from(T1 m1, T2 m2, T3 m3, T4 m4) {
		return new Quadruple<T1, T2, T3, T4>(m1, m2, m3, m4);
	}

	public static <T1, Rest> T1 get1(Tuple<T1, Rest> tuple) {
		return tuple.first;
	}

	public static <T1, T2, Rest> T2 get2(Tuple<T1, Tuple<T2, Rest>> tuple) {
		return tuple.rest.first;
	}

	public static <T1, T2, T3, Rest> T3 get3(Tuple<T1, Tuple<T2, Tuple<T3, Rest>>> tuple) {
		return tuple.rest.rest.first;
	}

	public static <T1, T2, T3, T4, Rest> T4 get4(Tuple<T1, Tuple<T2, Tuple<T3, Tuple<T4, Rest>>>> tuple) {
		return tuple.rest.rest.rest.first;
	}

	public static <T1, T2, T3, T4, T5, Rest> T5 get5(Tuple<T1, Tuple<T2, Tuple<T3, Tuple<T4, Tuple<T5, Rest>>>>> tuple) {
		return tuple.rest.rest.rest.rest.first;
	}

	public static <T1, T2, T3, T4, T5, T6, Rest> T6 get6(Tuple<T1, Tuple<T2, Tuple<T3, Tuple<T4, Tuple<T5, Tuple<T6, Rest>>>>>> tuple) {
		return tuple.rest.rest.rest.rest.rest.first;
	}

	public static <T1, T2, T3, T4, T5, T6, T7, Rest> T7 get7(Tuple<T1, Tuple<T2, Tuple<T3, Tuple<T4, Tuple<T5, Tuple<T6, Tuple<T7, Rest>>>>>>> tuple) {
		return tuple.rest.rest.rest.rest.rest.rest.first;
	}

	public static <T1, T2, T3, T4, T5, T6, T7, T8, Rest> T8 get8(Tuple<T1, Tuple<T2, Tuple<T3, Tuple<T4, Tuple<T5, Tuple<T6, Tuple<T7, Tuple<T8, Rest>>>>>>>> tuple) {
		return tuple.rest.rest.rest.rest.rest.rest.rest.first;
	}

	public static <T1, T2, T3, T4, T5, T6, T7, T8, T9, Rest> T9 get9(Tuple<T1, Tuple<T2, Tuple<T3, Tuple<T4, Tuple<T5, Tuple<T6, Tuple<T7, Tuple<T8, Tuple<T9, Rest>>>>>>>>> tuple) {
		return tuple.rest.rest.rest.rest.rest.rest.rest.rest.first;
	}

	public static <T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, Rest> T10 get10(Tuple<T1, Tuple<T2, Tuple<T3, Tuple<T4, Tuple<T5, Tuple<T6, Tuple<T7, Tuple<T8, Tuple<T9, Tuple<T10, Rest>>>>>>>>>> tuple) {
		return tuple.rest.rest.rest.rest.rest.rest.rest.rest.rest.first;
	}

}
