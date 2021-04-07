package org.katlas.JavaKh.algebra;

public interface Morphism<Obj, Mor extends Morphism<Obj, Mor>> {

	Obj source();
	Obj target();
	Mor compose(Mor m);
	
}
