package org.katlas.JavaKh.algebra;

public interface MatrixEntry<T> {
	T getValue();
	void setValue(T value);
	int getRow();
	int getColumn();
}

