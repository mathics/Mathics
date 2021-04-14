package org.katlas.JavaKh.algebra.implementations;

import org.katlas.JavaKh.algebra.LinearMorphism;
import org.katlas.JavaKh.algebra.Matrix;
import org.katlas.JavaKh.algebra.Ring;

public abstract class AbstractMatrix<R extends Ring<R>, Obj, Mor extends LinearMorphism<R, Obj, Mor>> implements Matrix<R, Obj, Mor> {

	public void addEntry(int row, int column, Mor t) {
		Mor existingEntry = getEntry(row, column);
		if(existingEntry == null) {
			putEntry(row, column, t);
		} else {
			putEntry(row, column, existingEntry.add(t));
		}
	}

//	public Matrix<R, Obj, Mor> extractColumns(List<Integer> columns) {
//		Matrix<R, Obj, Mor> result = null;
//		List<Integer> columnsExtracted = new ArrayList<Integer>();
//		for(int column : columns) {
//			int effectiveColumn = column;
//			for(int extractedColumn : columnsExtracted) {
//				if(extractedColumn < column) --effectiveColumn;
//			}
//			if(result == null) {
//				result = extractColumn(effectiveColumn);
//			} else {
//				result.insertAfterColumn(result.numberOfColumns(), extractColumn(effectiveColumn));
//			}
//			columnsExtracted.add(column);
//		}
//		return result;
//	}
//
//	public Matrix<R, Obj, Mor> extractRows(List<Integer> rows) {
//		Matrix<R, Obj, Mor> result = null;
//		List<Integer> rowsExtracted = new ArrayList<Integer>();
//		for(int row : rows) {
//			int effectiveRow = row;
//			for(int extractedRow : rowsExtracted) {
//				if(extractedRow < row) --effectiveRow;
//			}
//			if(result == null) {
//				result = extractRow(effectiveRow);
//			} else {
//				result.insertAfterRow(result.numberOfRows(), extractRow(effectiveRow));
//			}
//			rowsExtracted.add(row);
//		}
//		return result;
//	}

	
	
}
