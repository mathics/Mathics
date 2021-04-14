package org.katlas.JavaKh.test.MatrixRow;


import org.junit.Before;
import org.katlas.JavaKh.rows.ArrayRow;

public class ArrayRowTest extends MatrixRowTest<ArrayRow<String>> {

	@Before
	public void setUp() throws Exception {
		row = new ArrayRow<String>(12);
	}

}
