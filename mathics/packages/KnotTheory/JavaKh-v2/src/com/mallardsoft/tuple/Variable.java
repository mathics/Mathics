package com.mallardsoft.tuple;

public class Variable<T> {

	private T value;

	public Variable() {
		this.value = null;
	}

	public Variable(T value) {
		this.value = value;
	}

	public T get() {
		return value;
	}

	public void set(T value) {
		this.value = value;
	}

	public String toString() {
		if (value == null)
			return "null";
		else
			return value.toString();
	}

}
