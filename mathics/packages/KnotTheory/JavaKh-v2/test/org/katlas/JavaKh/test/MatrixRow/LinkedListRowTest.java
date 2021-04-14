package org.katlas.JavaKh.test.MatrixRow;


import org.junit.Before;
import org.katlas.JavaKh.rows.LinkedListRow;

public class LinkedListRowTest extends MatrixRowTest<LinkedListRow<String>> {

	@Before
	public void setUp() throws Exception {
		row = new LinkedListRow<String>();
	}

}
