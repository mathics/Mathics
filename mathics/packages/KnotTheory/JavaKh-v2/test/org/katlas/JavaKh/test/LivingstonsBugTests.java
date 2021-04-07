package org.katlas.JavaKh.test;

import static org.junit.Assert.assertEquals;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.StringReader;

import org.junit.BeforeClass;
import org.junit.Test;
import org.katlas.JavaKh.JavaKh;
import org.katlas.JavaKh.Komplex;
import org.katlas.JavaKh.algebra.rings.Int;
import org.katlas.JavaKh.algebra.rings.Rational;
import org.katlas.JavaKh.algebra.rings.Rings;


public class LivingstonsBugTests {

	private static final String pd = "PD[X[1, 8, 2, 9], X[3, 11, 4, 10], X[5, 12, 6, 13], X[7, 24, 8, 1], X[9, 18, 10, 19], X[11, 21, 12, 20], X[13, 6, 14, 7], X[22, 15, 23, 16], X[17, 3, 18, 2], X[19, 4, 20, 5], X[14, 21, 15, 22], X[16, 23, 17, 24]]";
	private static final String polynomial = "q^-23*t^-9 + 4*q^-21*t^-8 + q^-19*t^-8 + 6*q^-19*t^-7 + 4*q^-17*t^-7 + 9*q^-17*t^-6 + 6*q^-15*t^-6 + 11*q^-15*t^-5 + 9*q^-13*t^-5 + 10*q^-13*t^-4 + 11*q^-11*t^-4 + 10*q^-11*t^-3 + 10*q^-9*t^-3 + 6*q^-9*t^-2 + 10*q^-7*t^-2 + 4*q^-7*t^-1 + 6*q^-5*t^-1 + 2*q^-5*t^0 + 5*q^-3*t^0 + q^-1*t^1 ";
	
	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		JavaKh.inMemory = false;

		Rings.setRing("Rational");
	}
	
	private int[][] parseKnot(String pd) throws IOException {
		BufferedReader br = new BufferedReader(new StringReader(pd));
		return Komplex.getPD(br);
	}
	
	private Komplex<Rational> createComplex(int[][] knot, boolean reordering) {
		return (Komplex<Rational>) Komplex.generateFast(knot, Komplex.getSigns(knot), reordering, false, JavaKh.inMemory);
	}
	
	private Komplex<Rational> createComplex(String pd, boolean reordering) throws IOException {
		return createComplex(parseKnot(pd), reordering);
	}
	
	@Test(expected=UnsupportedOperationException.class)
	public void test() throws IOException {
		assertEquals(polynomial, createComplex(pd, false).Kh());
	}
	

}
