package com.mallardsoft.tuple;

public class Single<T1> extends Tuple<T1, End> {
	
	public Single(T1 m1) {
		super(m1, End.getInstance());
	}

}
