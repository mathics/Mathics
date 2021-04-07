package org.katlas.JavaKh.test.MatrixRow;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

import org.junit.Test;
import org.katlas.JavaKh.rows.MatrixRow;

public abstract class MatrixRowTest<Row extends MatrixRow<String>> {

	protected Row row;

	@Test
	public void testPut1() {
		row.put(2, "1");
		assertEquals("1", row.get(2));
		row.put(2, "2");
		assertEquals("2", row.get(2));
		row.put(3, "3");
		assertEquals("3", row.get(3));
		row.put(3, "4");
		assertEquals("4", row.get(3));
		row.put(2, "5");
		assertEquals("5", row.get(2));
		row.put(2, "6");
		assertEquals("6", row.get(2));
	}

	@Test
	public void testPut2() {
		Iterator<Integer> iterator;

		row.put(3, "3");
		row.put(2, "2");
		row.put(1, "1");

		iterator = row.keys().iterator();
		assertTrue(iterator.hasNext());
		assertTrue(1 == iterator.next());
		assertTrue(iterator.hasNext());
		assertTrue(2 == iterator.next());
		assertTrue(iterator.hasNext());
		assertTrue(3 == iterator.next());
		assertFalse(iterator.hasNext());
	}
	
	@Test
	public void testPut3() {
		row.put(1, "1");
		row.put(1, null);
		assertFalse(row.containsKey(1));
	}

	@Test
	public void testContainsKey() {
		row.put(3, "3");
		assertEquals("3", row.get(3));
	}

	@Test
	public void testDecrementIndexesAbove() {
		row.put(5, "5");
		row.put(6, "6");
		row.put(8, "8");
		row.decrementIndexesAbove(7);
		assertEquals("8", row.get(7));
		
		row.put(9, "9");
		assertEquals("9", row.get(9));
	}

	@Test
	public void testKeys() {
		List<Integer> ints = new ArrayList<Integer>();
		ints.add(5);
		ints.add(6);
		ints.add(8);
		ints.add(2);
		ints.add(4);
		for (int i : ints) {
			row.put(i, Integer.toString(i));
		}
		Collections.sort(ints);
		for (int k : row.keys()) {
			assertTrue(k == ints.get(0));
			ints.remove(0);
		}
	}

	@Test
	public void testRemove1() {
		row.put(1, "1");
		row.put(2, "2");
		row.remove(1);
		for (int i : row.keys()) {
			assertEquals("2", row.get(i));
		}
	}

	@Test
	public void testRemove2() {
		row.put(1, "1");
		row.put(2, "2");
		row.remove(2);
		for (int i : row.keys()) {
			assertEquals("1", row.get(i));
		}
	}

	@Test
	public void testRemove3() {
		Iterator<Integer> iterator;

		row.put(3, "3");
		row.put(2, "2");
		row.put(1, "1");

		row.remove(2);
		
		iterator = row.keys().iterator();
		assertTrue(iterator.hasNext());
		assertTrue(1 == iterator.next());
		assertTrue(iterator.hasNext());
		assertTrue(3 == iterator.next());
		assertFalse(iterator.hasNext());
	}

	@Test
	public void testPutLastRemove() {
		row.putLast(1, "1");
		row.putLast(2, "2");
		row.remove(2);
		row.putLast(3, "3");
		assertEquals("3", row.get(3));
	}
	
	@Test
	public void testDecrementRemove1() {
		row.put(1, "1");
		row.put(2, "2");
		row.put(3, "3");
		row.put(4, "4");
		row.remove(2);
		assertEquals(null, row.get(2));
		row.decrementIndexesAbove(2);
		assertEquals("3", row.get(2));
		row.remove(2);
		assertEquals(null, row.get(2));
		row.decrementIndexesAbove(2);
		assertEquals("4", row.get(2));
	}
	
	@Test
	public void testDecrementRemove2() {
		row.put(1, "1");
		row.put(2, "2");
		row.put(3, "3");
		row.put(4, "4");
		row.remove(2);
		row.decrementIndexesAbove(2);
		row.remove(2);
		row.decrementIndexesAbove(2);

		Iterator<Integer> iterator = row.keys().iterator();
		int key;
		assertTrue(iterator.hasNext());
		assertEquals(1, key = iterator.next());
		assertEquals("1", row.get(key));
		assertTrue(iterator.hasNext());
		assertEquals(2, key = iterator.next());
		assertEquals("4", row.get(key));
		assertFalse(iterator.hasNext());	
	}
	
	@Test
	public void testDecrementRemove3() {
		List<Integer> ints = Arrays.asList(new Integer[] {1,2,3,4,5,6,7,8,9});
		for (int i : ints) {
			row.put(i, Integer.toString(i));
		}
		
		row.remove(5);
		row.decrementIndexesAbove(5);
		row.remove(7);
		row.decrementIndexesAbove(7);
		row.remove(6);
		row.decrementIndexesAbove(6);
		row.remove(6);
		row.decrementIndexesAbove(6);

		Iterator<Integer> iterator = row.keys().iterator();
		int key;
		assertTrue(iterator.hasNext());
		assertEquals(1, key = iterator.next());
		assertEquals("1", row.get(key));
		assertTrue(iterator.hasNext());
		assertEquals(2, key = iterator.next());
		assertEquals("2", row.get(key));
		assertTrue(iterator.hasNext());
		assertEquals(3, key = iterator.next());
		assertEquals("3", row.get(key));
		assertTrue(iterator.hasNext());
		assertEquals(4, key = iterator.next());
		assertEquals("4", row.get(key));
		assertTrue(iterator.hasNext());
		assertEquals(5, key = iterator.next());
		assertEquals("6", row.get(key));
		assertFalse(iterator.hasNext());	
		
	}

	@Test
	public void testPutLast() {
		row.putLast(1, "1");
		row.putLast(2, "2");
		row.putLast(4, "4");

		row.remove(2);
		
		Iterator<Integer> iterator = row.keys().iterator();
		assertTrue(iterator.hasNext());
		assertEquals("1", row.get(iterator.next()));
		assertTrue(iterator.hasNext());
		assertEquals("4", row.get(iterator.next()));
		assertFalse(iterator.hasNext());
	}

	
}
