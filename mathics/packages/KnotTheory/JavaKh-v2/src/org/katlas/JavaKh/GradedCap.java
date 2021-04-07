package org.katlas.JavaKh;

public class GradedCap {

	private final Cap cap;
	private final int grading;
	
	GradedCap(Cap cap, int grading) {
		super();
		this.cap = cap;
		this.grading = grading;
	}

	public Cap getCap() {
		return cap;
	}

	public int getGrading() {
		return grading;
	}

}
