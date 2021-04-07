package org.katlas.JavaKh.test.MatrixRow;


import org.junit.Before;
import org.katlas.JavaKh.rows.PackedArrayRow;

public class PackedArrayRowTest extends MatrixRowTest<PackedArrayRow<String>> {

	@Before
	public void setUp() throws Exception {
		row = new PackedArrayRow<String>();
	}

}
