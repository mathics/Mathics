package org.katlas.JavaKh.test.MatrixRow;


import org.junit.Before;
import org.katlas.JavaKh.rows.RedBlackIntegerMap;

public class RedBlackIntegerMapTest extends MatrixRowTest<RedBlackIntegerMap<String>> {

	@Before
	public void setUp() throws Exception {
		row = new RedBlackIntegerMap<String>();
	}

}
