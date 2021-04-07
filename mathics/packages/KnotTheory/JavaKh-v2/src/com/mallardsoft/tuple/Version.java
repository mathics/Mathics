package com.mallardsoft.tuple;

public class Version extends Quadruple<Integer, Integer, Integer, Integer> {
    
    // A version is a tuple of 4 integers separated by periods.
    public Version(int major, int minor, int release, int qfe) {
    	super(major, minor, release, qfe);
    }
    
    public String toString() {
    	return toString("", ".", "");
    }
    
}
