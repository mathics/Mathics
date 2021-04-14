package org.katlas.JavaKh.algebra.rings;

import java.lang.reflect.Constructor;
import java.util.HashMap;
import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.katlas.JavaKh.algebra.Ring;

public class Rings<R extends Ring<R>> {
	private static final Log log = LogFactory.getLog(Rings.class);

	public final R ZERO, ONE, MINUSONE;
	
	private final Constructor<R> constructor;
	public final String name;
	
	private static Map<String, Rings<?>> map = new HashMap<String, Rings<?>>();
	private static Rings<?> current;
	
	@SuppressWarnings("unchecked")
	public static <R extends Ring<R>> Rings<R> current() {
		return (Rings<R>) current;
	}
	
	@SuppressWarnings("unchecked")
	public static void setRing(String ringName) {
		if(map.get(ringName) == null) {
			map.put(ringName, new Rings(ringName));
		}
		current = map.get(ringName);
	}
	
	@SuppressWarnings("unchecked")
	public static <R extends Ring<R>> Rings<R> getRing(String ringName) {
		if(map.get(ringName) == null) {
			map.put(ringName, new Rings<R>(ringName));
		}
		current = map.get(ringName);
		return (Rings<R>)current;
	}
	
	@SuppressWarnings("unchecked")
	private Rings(String ringName) {
		name = ringName;
		Constructor<R> constructor;
		try {
		    Class<?> params[] = {Integer.TYPE};
		    Class<R> ringClass = (Class<R>) Class.forName("org.katlas.JavaKh.algebra.rings." + name);
		    constructor = ringClass.getConstructor(params);
		} catch (Exception e) {
			constructor = null;
			System.err.println("Error setting BaseRing");
		    System.exit(1);
		}
		this.constructor = constructor;
		ZERO = createInstance(0);
		ONE = createInstance(1);
		MINUSONE = createInstance(-1);
	}
	
	public R createInstance(int i) {
		assert constructor != null;
		try {
			return (constructor.newInstance(i));
		} catch (Exception e) {
			log.warn(e);
			throw new RuntimeException(e);
		} 
	}
}