package com.mallardsoft.tuple;

public class Triple<T1, T2, T3> extends Tuple<T1, Tuple<T2, Tuple<T3, End>>> {

	public Triple(T1 m1, T2 m2, T3 m3) {
		super(m1, Tuple.from(m2, m3));
	}
	
}
