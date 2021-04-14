package org.katlas.JavaKh.test.MatrixRow;


import org.junit.Before;
import org.katlas.JavaKh.rows.MaskedArrayRow;

public class MaskedArrayRowTest extends MatrixRowTest<MaskedArrayRow<String>> {

	@Before
	public void setUp() throws Exception {
		row = new MaskedArrayRow<String>(12);
	}

}
