package org.katlas.JavaKh.test;

import static org.junit.Assert.assertEquals;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.StringReader;

import org.junit.BeforeClass;
import org.junit.Test;
import org.katlas.JavaKh.BaseRing;
import org.katlas.JavaKh.Komplex;


public class BigTorusKnotTests {

	private static final String pdT76 = "PD[X[13, 25, 14, 24], X[3, 15, 4, 14], X[2, 26, 3, 25], X[12, 36, 13, 35], X[1, 37, 2, 36], X[23, 35, 24, 34], X[70, 48, 1, 47], X[11, 47, 12, 46], X[22, 46, 23, 45], X[33, 45, 34, 44], X[10, 58, 11, 57], X[21, 57, 22, 56], X[32, 56, 33, 55], X[43, 55, 44, 54], X[69, 59, 70, 58], X[59, 49, 60, 48], X[60, 38, 61, 37], X[61, 27, 62, 26], X[49, 39, 50, 38], X[50, 28, 51, 27], X[62, 16, 63, 15], X[63, 5, 64, 4], X[51, 17, 52, 16], X[52, 6, 53, 5], X[53, 65, 54, 64], X[39, 29, 40, 28], X[40, 18, 41, 17], X[42, 66, 43, 65], X[41, 7, 42, 6], X[9, 69, 10, 68], X[20, 68, 21, 67], X[31, 67, 32, 66], X[29, 19, 30, 18], X[19, 9, 20, 8], X[30, 8, 31, 7]]";
	private static final String T76_polynomial = "q^29*t^0 + q^31*t^0 + q^33*t^2 + q^37*t^3 + q^35*t^4 + q^37*t^4 + q^39*t^5 + q^41*t^5 + q^37*t^6 + q^39*t^6 + q^41*t^7 + q^43*t^7 + q^39*t^8 + 2*q^41*t^8 + q^43*t^9 + 2*q^45*t^9 + q^41*t^10 + 2*q^43*t^10 + q^45*t^11 + 3*q^47*t^11 + 2*q^45*t^12 + q^47*t^12 + q^51*t^12 + 3*q^49*t^13 + q^51*t^13 + q^47*t^14 + q^49*t^14 + q^53*t^14 + 2*q^51*t^15 + 2*q^53*t^15 + q^49*t^16 + q^51*t^16 + q^55*t^16 + q^57*t^16 + q^53*t^17 + q^55*t^17 + q^53*t^18 + q^57*t^19 ";
	
	private static final String pdT87 = "PD[X[32, 60, 33, 59], X[33, 47, 34, 46], X[45, 59, 46, 58], X[44, 72, 45, 71], X[31, 73, 32, 72], X[57, 71, 58, 70], X[19, 61, 20, 60], X[18, 74, 19, 73], X[20, 48, 21, 47], X[21, 35, 22, 34], X[6, 62, 7, 61], X[7, 49, 8, 48], X[5, 75, 6, 74], X[8, 36, 9, 35], X[9, 23, 10, 22], X[30, 86, 31, 85], X[43, 85, 44, 84], X[56, 84, 57, 83], X[17, 87, 18, 86], X[69, 83, 70, 82], X[4, 88, 5, 87], X[88, 76, 89, 75], X[89, 63, 90, 62], X[76, 64, 77, 63], X[90, 50, 91, 49], X[77, 51, 78, 50], X[91, 37, 92, 36], X[78, 38, 79, 37], X[92, 24, 93, 23], X[79, 25, 80, 24], X[93, 11, 94, 10], X[80, 12, 81, 11], X[81, 95, 82, 94], X[64, 52, 65, 51], X[16, 4, 17, 3], X[68, 96, 69, 95], X[67, 13, 68, 12], X[66, 26, 67, 25], X[65, 39, 66, 38], X[55, 1, 56, 96], X[52, 40, 53, 39], X[54, 14, 55, 13], X[53, 27, 54, 26], X[40, 28, 41, 27], X[28, 16, 29, 15], X[41, 15, 42, 14], X[42, 2, 43, 1], X[29, 3, 30, 2]]";
	private static final String T87_polynomial = "q^41*t^0 + q^43*t^0 + q^45*t^2 + q^49*t^3 + q^47*t^4 + q^49*t^4 + q^51*t^5 + q^53*t^5 + q^49*t^6 + q^51*t^6 + q^53*t^7 + q^55*t^7 + q^51*t^8 + 2*q^53*t^8 + q^55*t^9 + 2*q^57*t^9 + q^53*t^10 + 2*q^55*t^10 + q^57*t^11 + 3*q^59*t^11 + q^55*t^12 + 3*q^57*t^12 + q^59*t^12 + q^63*t^12 + q^59*t^13 + 4*q^61*t^13 + q^63*t^13 + 2*q^59*t^14 + q^61*t^14 + q^65*t^14 + 4*q^63*t^15 + 2*q^65*t^15 + 2*q^61*t^16 + 2*q^63*t^16 + 2*q^67*t^16 + q^69*t^16 + 3*q^65*t^17 + 3*q^67*t^17 + q^63*t^18 + 2*q^65*t^18 + q^69*t^18 + q^71*t^18 + 2*q^67*t^19 + 3*q^69*t^19 + q^65*t^20 + 2*q^67*t^20 + q^71*t^20 + q^73*t^20 + q^69*t^21 + 3*q^71*t^21 + q^69*t^22 + q^75*t^22 + 2*q^73*t^23 + q^71*t^24 + q^73*t^24 + q^77*t^24 + q^75*t^25 + q^77*t^25 ";
	
	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		BaseRing.setRing("Rational");
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
	public void testT76() throws IOException {
		assertEquals(T76_polynomial, createComplex(pdT76).Kh());
	}

	@Test
	public void testT87() throws IOException {
		assertEquals(T87_polynomial, createComplex(pdT87).Kh());
	}

}
