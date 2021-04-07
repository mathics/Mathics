package org.katlas.JavaKh;

public abstract class BaseRing {
    private static java.lang.reflect.Constructor constructor;
    public static String ring;
    public static void setRing(String cls) {
	ring = cls;
	try {
	    Class params[] = {Integer.TYPE};
	    constructor = Class.forName("org.katlas.JavaKh." + cls).getConstructor(params);
	} catch (Exception e) {
	    System.err.println("Error setting BaseRing");
	    System.exit(1);
	}
    }

    public static BaseRing fromInt(int n) {
	try {
	    Object params[] = {new Integer(n)};
	    return (BaseRing) constructor.newInstance(params);
	} catch (Exception e) {
	    System.err.println("Error instantiating BaseRing");
	    System.exit(1);
	    return null; // make the compiler stop complaining
	}
    }

    public abstract boolean isInvertible();

    public abstract BaseRing inverse();

    public abstract BaseRing multiply(BaseRing br);

    public abstract BaseRing multiply(int n);

    public abstract BaseRing add(BaseRing br);

    public abstract boolean isZero();
}
