package com.mallardsoft.tuple;

public class End implements SeparatedAppender, Comparable<End> {
	
	private static End instance = new End();
	
	public static End getInstance() {
		return instance;
	}

	private End() { }

	public void appendString(StringBuffer buffer, String separator) {
	}

	public int compareTo(End o) {
		return 0;
	}

}
