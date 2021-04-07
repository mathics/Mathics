package org.katlas.JavaKh.test.MatrixRow;


import org.junit.Before;
import org.katlas.JavaKh.rows.DoublyLinkedListRow;

public class DoublyLinkedListRowTest extends MatrixRowTest<DoublyLinkedListRow<String>> {

	@Before
	public void setUp() throws Exception {
		row = new DoublyLinkedListRow<String>();
	}

}
