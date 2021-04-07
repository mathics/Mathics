package org.katlas.JavaKh.algebra;

import java.util.List;


public interface Matrix<R extends Ring<R>, Obj, Mor extends LinearMorphism<R, Obj, Mor>>
	extends LinearMorphism<R, DirectSum<Obj>, Matrix<R, Obj, Mor>>,
			Iterable<MatrixEntry<Mor>>
{

	public int numberOfRows();
	public int numberOfColumns();
	
	public Mor getEntry(int row, int column);
	public void putEntry(int row, int column, Mor t);
	public void addEntry(int row, int column, Mor t);
	
	public Matrix<R, Obj, Mor> extractRow(int row);
	public Matrix<R, Obj, Mor> extractColumn(int column);

	public Matrix<R, Obj, Mor> extractRows(List<Integer> rows);
	public Matrix<R, Obj, Mor> extractColumns(List<Integer> columns);
	
//	public void insertAfterRow(int row, Matrix<R, Obj, Mor> extraRows);
//	public void insertAfterColumn(int column, Matrix<R, Obj, Mor> extraColumns);
	
	public Matrix<R, Obj, Mor> compose(Matrix<R, Obj, Mor> matrix);
	
	public Iterable<? extends MatrixEntry<Mor>> matrixRowEntries(int row);
	public Iterable<? extends MatrixEntry<Mor>> matrixColumnEntries(int column);
	
}

