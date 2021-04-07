package org.katlas.JavaKh.test;



import static org.junit.Assert.assertEquals;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.StringReader;

import org.junit.BeforeClass;
import org.junit.Test;
import org.katlas.JavaKh.BaseRing;
import org.katlas.JavaKh.Komplex;


public class IntTorusKnotTests {

	private static final String pdT43 = "PD[X[4,16,5,15],X[5,11,6,10],X[16,12,1,11],X[1,7,2,6],X[12,8,13,7],X[9,15,10,14],X[13,3,14,2],X[8,4,9,3]]";
	private static final String T43_zmodule = "q^5*t^0*Z[0] + q^7*t^0*Z[0] + q^9*t^2*Z[0] + q^11*t^3*Z[2] + q^13*t^3*Z[0] + q^11*t^4*Z[0] + q^13*t^4*Z[0] + q^15*t^5*Z[0] + q^17*t^5*Z[0]";
	
	private static final String pdT65 = "PD[X[1, 31, 2, 30], X[48, 40, 1, 39], X[40, 32, 41, 31], X[10, 30, 11, 29], X[9, 39, 10, 38], X[8, 48, 9, 47], X[11, 21, 12, 20], X[19, 29, 20, 28], X[2, 22, 3, 21], X[3, 13, 4, 12], X[18, 38, 19, 37], X[17, 47, 18, 46], X[41, 23, 42, 22], X[16, 8, 17, 7], X[32, 24, 33, 23], X[42, 14, 43, 13], X[33, 15, 34, 14], X[24, 16, 25, 15], X[43, 5, 44, 4], X[34, 6, 35, 5], X[25, 7, 26, 6], X[35, 45, 36, 44], X[26, 46, 27, 45], X[27, 37, 28, 36]]";
	private static final String T65_zmodule = "q^19*t^0*Z[0] + q^21*t^0*Z[0] + q^23*t^2*Z[0] + q^25*t^3*Z[2] + q^27*t^3*Z[0] + q^25*t^4*Z[0] + q^27*t^4*Z[0] + q^29*t^5*Z[0] + q^31*t^5*Z[0] + q^27*t^6*Z[0] + q^29*t^6*Z[0] + q^29*t^7*Z[2] + q^31*t^7*Z[0,2] + q^33*t^7*Z[0] + q^29*t^8*Z[0] + q^31*t^8*Z[0,0] + q^33*t^9*Z[0,2] + q^35*t^9*Z[0,0] + q^33*t^10*Z[0] + q^35*t^10*Z[2] + q^37*t^10*Z[2] + q^35*t^11*Z[2,5] + q^37*t^11*Z[0,0] + q^35*t^12*Z[0] + q^37*t^12*Z[0] + q^39*t^12*Z[2,5] + q^41*t^12*Z[0] + q^39*t^13*Z[0] + q^41*t^13*Z[0] + q^43*t^14*Z[3]";
	
	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		BaseRing.setRing("Int");
	}
	
	private int[][] parseKnot(String pd) throws IOException {
		BufferedReader br = new BufferedReader(new StringReader(pd));
		return Komplex.getPD(br);
	}
	
	private Komplex createComplex(int[][] knot) {
		return Komplex.generateFast(knot, Komplex.getSigns(knot));
	}
	
	private Komplex createComplex(String pd) throws IOException {
		return createComplex(parseKnot(pd));
	}
	
	@Test
	public void testT43() throws IOException {
		assertEquals(T43_zmodule, createComplex(pdT43).KhForZ());
	}

	@Test
	public void testT65() throws IOException {
		assertEquals(T65_zmodule, createComplex(pdT65).KhForZ());
	}

}
