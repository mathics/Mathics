//package org.katlas.JavaKh.algebra.implementations;
//
//import java.util.ArrayList;
//import java.util.Collections;
//import java.util.Iterator;
//import java.util.List;
//import java.util.Map;
//import java.util.TreeMap;
//
//import org.katlas.JavaKh.algebra.DirectSum;
//import org.katlas.JavaKh.algebra.LinearMorphism;
//import org.katlas.JavaKh.algebra.Matrix;
//import org.katlas.JavaKh.algebra.MatrixEntry;
//import org.katlas.JavaKh.algebra.Ring;
//
//import net.tqft.iterables.AbstractIterator;
//import net.tqft.iterables.ForgetfulIteratorBundle;
//
//public class SparseMatrix<R extends Ring<R>, Obj, Mor extends LinearMorphism<R, Obj, Mor>> extends
//		AbstractMatrix<R, Obj, Mor> implements Matrix<R, Obj, Mor> {
//
//	List<Label> rows, columns;
//	Map<Label, SparseMatrixEntry<Mor>> initialRowEntries, initialColumnEntries;
//
//	public SparseMatrix(int numberOfRows, int numberOfColumns) {
//		rows = fillLabels(numberOfRows);
//		columns = fillLabels(numberOfColumns);
//		initialRowEntries = new TreeMap<Label, SparseMatrixEntry<Mor>>();
//		initialColumnEntries = new TreeMap<Label, SparseMatrixEntry<Mor>>();
//	}
//	
//	public SparseMatrix(List<Label> rows, List<Label> columns) {
//		this.rows = rows;
//		this.columns = columns;
//		initialRowEntries = new TreeMap<Label, SparseMatrixEntry<Mor>>();
//		initialColumnEntries = new TreeMap<Label, SparseMatrixEntry<Mor>>();
//	}
//
//	public SparseMatrix(Matrix<R, Obj, Mor> matrix) {
//		rows = fillLabels(matrix.numberOfRows());
//		columns = fillLabels(matrix.numberOfColumns());
//		initialRowEntries = new TreeMap<Label, SparseMatrixEntry<Mor>>();
//		initialColumnEntries = new TreeMap<Label, SparseMatrixEntry<Mor>>();
//		
//		for(MatrixEntry<Mor> entry: matrix) {
//			putEntry(entry.getRow(), entry.getColumn(), entry.getValue());
//		}
//	}
//
//	private List<Label> fillLabels(int i) {
//		List<Label> result = new ArrayList<Label>();
//		for(int index = 0; index < i; ++index) {
//			result.add(new Label(index));
//		}		
//	
//		return result;
//	}	
//	
//	/*
//	 * During composition, we rely on the rows and columns actually being numbered sequentially.
//	 * The insert operations can break this, so they need to call reorderRows and reorderColumns.
//	 */
//	public Matrix<R, Obj, Mor> compose(Matrix<R, Obj, Mor> matrix) {
//		List<Label> resultRows = rows;
//		List<Label> resultColumns;
//		if(matrix instanceof SparseMatrix) {
//			resultColumns = ((SparseMatrix<R, Obj, Mor>)matrix).columns;
//		} else {
//			resultColumns = fillLabels(matrix.numberOfColumns());
//		}
//		Matrix<R, Obj, Mor> result = new SparseMatrix<R, Obj, Mor>(resultRows, resultColumns);
//		for(int row = 0; row < numberOfRows(); ++row) {
//			for(int column = 0; column < matrix.numberOfColumns(); ++column) {
//				Mor t = null;
//				Iterator<? extends MatrixEntry<Mor>> columnIterator = matrix.matrixColumnEntries(column).iterator();
//				if(!columnIterator.hasNext()) continue;
//				MatrixEntry<Mor> columnEntry = columnIterator.next();
//				for(SparseMatrixEntry<Mor> rowEntry : matrixRowEntries(row)) {
//					while(columnEntry.getRow() < rowEntry.getColumn()) {
//						if(columnIterator.hasNext()) {
//							columnEntry = columnIterator.next();
//						} else {
//							columnEntry = null;
//						}
//					}
//					if(columnEntry == null) break;
//					if(columnEntry.getRow() == rowEntry.getColumn()) {
//						// found something!
//						Mor product = rowEntry.value.compose(columnEntry.getValue());
//						if(t == null) {
//							t = product;
//						} else {
//							t = t.add(product);
//						}
//					}
//				}
//				if(t != null) {
//					result.putEntry(row, column, t);
//				}
//			}
//		}
//		
//		return result;
//	}
//
//	public Matrix<R, Obj, Mor> extractColumn(int columnIndex) {
//		if(columnIndex < 0 || columnIndex >= columns.size()) {
//			throw new ArrayIndexOutOfBoundsException();
//		}
//
//		Label column = columns.get(columnIndex);
//		
//		SparseMatrix<R, Obj, Mor> result = new SparseMatrix<R, Obj, Mor>(rows,
//				Collections.singletonList(column));
//		SparseMatrixEntry<Mor> columnEntry = initialColumnEntries.get(column);
//		columns.remove(column);
//
//		if (columnEntry != null) {
//			result.initialColumnEntries.put(column, columnEntry);
//			while (columnEntry != null) {
//				if (initialRowEntries.get(columnEntry.row) == columnEntry) {
//					initialRowEntries.put(columnEntry.row, columnEntry.right);
//				}
//				if (columnEntry.left != null) {
//					columnEntry.left.right = columnEntry.right;
//				}
//				if (columnEntry.right != null) {
//					columnEntry.right.left = columnEntry.left;
//				}
//				columnEntry.left = null;
//				columnEntry.right = null;
//				result.initialRowEntries.put(columnEntry.row, columnEntry);
//				columnEntry = columnEntry.down;
//			}
//		}
//
//		reindexColumns();
//		
//		return result;
//	}
//
//	public Matrix<R, Obj, Mor> extractRow(int rowIndex) {
//		if(rowIndex < 0 || rowIndex >= rows.size()) {
//			throw new ArrayIndexOutOfBoundsException();
//		}
//		
//		Label row = rows.get(rowIndex);
//	
//		SparseMatrix<R, Obj, Mor> result = new SparseMatrix<R, Obj, Mor>(columns,
//				Collections.singletonList(row));
//		SparseMatrixEntry<Mor> rowEntry = initialRowEntries.get(row);
//		rows.remove(row);
//
//		if (rowEntry != null) {
//			result.initialRowEntries.put(row, rowEntry);
//			while (rowEntry != null) {
//				if (initialColumnEntries.get(rowEntry.column) == rowEntry) {
//					initialColumnEntries.put(rowEntry.column, rowEntry.down);
//				}
//				if (rowEntry.up != null) {
//					rowEntry.up.down = rowEntry.down;
//				}
//				if (rowEntry.down != null) {
//					rowEntry.down.up = rowEntry.up;
//				}
//				rowEntry.up = null;
//				rowEntry.down = null;
//				result.initialColumnEntries.put(rowEntry.column, rowEntry);
//				rowEntry = rowEntry.right;
//			}
//		}
//
//		reindexRows();
//
//		return result;
//	}
//
//	private void reindexColumns() {
//		for(int i = 0; i < columns.size(); ++i) {
//			columns.get(i).index = i;
//		}
//	}
//
//	private void reindexRows() {
//		for(int i = 0; i < rows.size(); ++i) {
//			rows.get(i).index = i;
//		}
//	}
//	
//	public void insertAfterColumn(int column, Matrix<R, Obj, Mor> extraColumns_) {
//		if(!(extraColumns_ instanceof SparseMatrix)) {
//			insertAfterColumn(column, new SparseMatrix<R, Obj, Mor>(extraColumns_));
//		}
//		SparseMatrix<R, Obj, Mor> extraColumns = (SparseMatrix<R, Obj, Mor>)extraColumns_;
//		
//		for(Label row : rows) {
//			SparseMatrixEntry<Mor> first = extraColumns.initialRowEntries.get(row);
//			SparseMatrixEntry<Mor> last = first;
//			if(last == null) {
//				continue;
//			} else {
//				while(last.right != null) {
//					last = last.right;
//				}
//			}
//			
//			SparseMatrixEntry<Mor> stitch = initialRowEntries.get(row);
//			if(stitch == null) {
//				initialRowEntries.put(row, first);
//			} else {
//				while(stitch.right != null & stitch.getColumn() < column) {
//					stitch = stitch.right;	
//				}
//				last.right = stitch.right;
//				stitch.right.left = last;
//				
//				stitch.right = first;
//				first.left = stitch;
//			}
//		}
//	}
//
//	public void insertAfterRow(int row, Matrix<R, Obj, Mor> extraRows_) {
//		if(!(extraRows_ instanceof SparseMatrix)) {
//			insertAfterRow(row, new SparseMatrix<R, Obj, Mor>(extraRows_));
//		}
//		SparseMatrix<R, Obj, Mor> extraRows = (SparseMatrix<R, Obj, Mor>)extraRows_;
//
//		for(Label column : columns) {
//			SparseMatrixEntry<Mor> first = extraRows.initialColumnEntries.get(column);
//			SparseMatrixEntry<Mor> last = first;
//			if(last == null) {
//				continue;
//			} else {
//				while(last.down != null) {
//					last = last.down;
//				}
//			}
//			
//			SparseMatrixEntry<Mor> stitch = initialColumnEntries.get(column);
//			if(stitch == null) {
//				initialColumnEntries.put(column, first);
//			} else {
//				while(stitch.down != null & stitch.getColumn() < row) {
//					stitch = stitch.down;	
//				}
//				last.down = stitch.down;
//				stitch.down.up = last;
//				
//				stitch.down = first;
//				first.up = stitch;
//			}
//		}
//
//	}
//
//	public int numberOfColumns() {
//		return columns.size();
//	}
//
//	public int numberOfRows() {
//		return rows.size();
//	}
//
//	private void checkIndexes(int rowIndex, int columnIndex) {
//		if(rowIndex < 0 || rowIndex >= rows.size()) {
//			throw new ArrayIndexOutOfBoundsException();
//		}
//		if(columnIndex < 0 || columnIndex >= columns.size()) {
//			throw new ArrayIndexOutOfBoundsException();
//		}
//	}
//	
//	public void putEntry(int rowIndex, int columnIndex, Mor t) {
//		checkIndexes(rowIndex, columnIndex);
//		put(rows.get(rowIndex), columns.get(columnIndex), t, false);
//	}
//
//	public void addEntry(int rowIndex, int columnIndex, Mor t) {
//		checkIndexes(rowIndex, columnIndex);
//		put(rows.get(rowIndex), columns.get(columnIndex), t, true);
//	}
//
//	private void put(Label row, Label column, Mor t, boolean addition) {
//		SparseMatrixEntry<Mor> newMatrixEntry = new SparseMatrixEntry<Mor>(row, column);
//
//		boolean columnRequiresLinking = true;
//
//		{
//			SparseMatrixEntry<Mor> currentRowEntry = initialRowEntries.get(row);
//			if (currentRowEntry == null) {
//				// the whole row is empty
//				initialRowEntries.put(row, newMatrixEntry);
//				newMatrixEntry.value = t;
//			} else {
//				while (currentRowEntry.right != null
//						&& currentRowEntry.column.index < column.index) {
//					currentRowEntry = currentRowEntry.right;
//				}
//				if (currentRowEntry.column.index == column.index) {
//					// the matrix entry already exists
//					columnRequiresLinking = false;
//					if (addition) {
//						currentRowEntry.value.add(t);
//					} else {
//						currentRowEntry.value = t;
//					}
//				} else {
//					// the matrix entry doesn't exist; we need to link it in
//					// both the row and the column
//					newMatrixEntry.value = t;
//					newMatrixEntry.left = currentRowEntry;
//					newMatrixEntry.right = currentRowEntry.right;
//					if (currentRowEntry.right != null) {
//						currentRowEntry.right.left = newMatrixEntry;
//					}
//					currentRowEntry.right = newMatrixEntry;
//				}
//			}
//		}
//		if (columnRequiresLinking) {
//			SparseMatrixEntry<Mor> currentColumnEntry = initialColumnEntries
//					.get(column);
//			if (currentColumnEntry == null) {
//				initialColumnEntries.put(column, newMatrixEntry);
//			} else {
//				while (currentColumnEntry.down != null
//						&& currentColumnEntry.row.index < row.index) {
//					currentColumnEntry = currentColumnEntry.down;
//				}
//				if (currentColumnEntry.row.index == row.index) {
//					throw new AssertionError(
//							"Failed to find the matrix entry in a row, but it later turned up in a column!");
//				} else {
//					newMatrixEntry.up = currentColumnEntry;
//					newMatrixEntry.down = currentColumnEntry.down;
//					if (currentColumnEntry.down != null) {
//						currentColumnEntry.down.up = newMatrixEntry;
//					}
//					currentColumnEntry.down = newMatrixEntry;
//				}
//			}
//		}
//
//	}
//
//	// WARNING: adds in place.
//	public Matrix<R, Obj, Mor> add(Matrix<R, Obj, Mor> m) {
//		// this is a pretty crappy implementation!
//		for(MatrixEntry<Mor> entry : m) {
//			addEntry(entry.getRow(), entry.getColumn(), entry.getValue());
//		}
//		return this;
//	}
//
//	// WARNING: multiplies in place.
//	public Matrix<R, Obj, Mor> multiply(R r) {
//		for(Label row : rows) {
//			SparseMatrixEntry<Mor> entry = initialRowEntries.get(row);
//			while(entry != null) {
//				entry.value = entry.value.multiply(r);
//				entry = entry.right;
//			}
//		}
//		return this;
//	}
//
//
//	public Iterator<MatrixEntry<Mor>> iterator() {
//		return new ForgetfulIteratorBundle<Label, MatrixEntry<Mor>, SparseMatrixEntry<Mor>>(rows.iterator()) {
//			@Override
//			protected Iterator<SparseMatrixEntry<Mor>> buildNewFibreIterator(
//					Label row) {
//				return matrixRowEntries(row.index).iterator();
//			}
//		};
//	}
//	
////	public Iterable<MatrixEntry<Mor>> matrixEntries() {
////		return new ForgetfulIterableBundle<Label, MatrixEntry<Mor>, SparseMatrixEntry<Mor>>(rows) {
////			@Override
////			protected Iterable<SparseMatrixEntry<Mor>> buildNewFibreIterable(
////					Label row) {
////				return matrixRowEntries(row.index);
////			}
////		};
////	}
//
//	public Iterable<SparseMatrixEntry<Mor>> matrixColumnEntries(final int column) {
//		return new Iterable<SparseMatrixEntry<Mor>>() {
//			public Iterator<SparseMatrixEntry<Mor>> iterator() {
//				return new AbstractIterator<SparseMatrixEntry<Mor>>() {
//					SparseMatrixEntry<Mor> entry = initialColumnEntries.get(columns.get(column));
//					public boolean hasNext() {
//						return entry != null;
//					}
//					protected SparseMatrixEntry<Mor> returnNext() {
//						SparseMatrixEntry<Mor> result = entry;
//						entry = entry.down;
//						return result;
//					}		
//				};
//			}
//		};
//	}
//
//	public Iterable<SparseMatrixEntry<Mor>> matrixRowEntries(final int row) {
//		return new Iterable<SparseMatrixEntry<Mor>>() {
//			public Iterator<SparseMatrixEntry<Mor>> iterator() {
//				return new AbstractIterator<SparseMatrixEntry<Mor>>() {
//					SparseMatrixEntry<Mor> entry = initialRowEntries.get(rows.get(row));
//					public boolean hasNext() {
//						return entry != null;
//					}
//					protected SparseMatrixEntry<Mor> returnNext() {
//						SparseMatrixEntry<Mor> result = entry;
//						entry = entry.right;
//						return result;
//					}		
//				};
//			}
//		};
//	}
//
//
//	class SparseMatrixEntry<T> implements MatrixEntry<T> {
//		SparseMatrixEntry(Label row, Label column) {
//			this.row = row;
//			this.column = column;
//		}
//		
//		T value;
//		Label row, column;
//		SparseMatrixEntry<T> up, down, left, right;
//		
//		public int getColumn() {
//			return column.index;
//		}
//		public int getRow() {
//			return row.index;
//		}
//		public T getValue() {
//			return value;
//		}
//		public void setValue(T value) {
//			this.value = value;
//		}
//	}
//
//
//	class Label implements Comparable<Label> {
//		int index;
//		Label(int index) {
//			this.index = index;
//		}
//		public int compareTo(Label l) {
//			return index - l.index;
//		}
//	}
//
//
//	public Mor getEntry(int row, int column) {
//		// TODO
//		throw new UnsupportedOperationException();
//	}
//
//	public DirectSum<Obj> source() {
//		// TODO Auto-generated method stub
//		return null;
//	}
//
//	public DirectSum<Obj> target() {
//		// TODO Auto-generated method stub
//		return null;
//	}
//
//
//}
