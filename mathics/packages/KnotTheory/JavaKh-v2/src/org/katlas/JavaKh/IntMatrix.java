package org.katlas.JavaKh;

import java.math.BigInteger;
import java.util.List;

import org.katlas.JavaKh.algebra.rings.Int;
import org.katlas.JavaKh.interfaces.LCCC;

public class IntMatrix {
	int rows, columns;
	BigInteger matrix[][];
	IntMatrix prev, next;
	List<Integer> source, target;

	public IntMatrix(int r, int c) {
		rows = r;
		columns = c;
		matrix = new BigInteger[r][c];
		for (int i = 0; i < rows; i++)
			for (int j = 0; j < columns; j++)
				matrix[i][j] = BigInteger.ZERO;
	}

	public IntMatrix(CobMatrix<Int> cm) {
		rows = cm.target.n;
		columns = cm.source.n;
		matrix = new BigInteger[rows][columns];

		for (int i = 0; i < rows; i++) {
			LCCC<Int> rowi[] = cm.unpackRow(i);
			for (int j = 0; j < columns; j++)
				if (rowi[j] == null || rowi[j].numberOfTerms() == 0)
					matrix[i][j] = BigInteger.ZERO;
				else {
					assert rowi[j].numberOfTerms() == 1;
					matrix[i][j] = ((Int) rowi[j].firstCoefficient()).getN();
				}
		}
	}

	public boolean isDiagonal() {
		for (int i = 0; i < rows; i++)
			for (int j = 0; j < columns; j++)
				if (!matrix[i][j].equals(BigInteger.ZERO) && i != j)
					return false;
		return true;
	}

	public boolean isZero() {
		for (int i = 0; i < rows; i++)
			for (int j = 0; j < columns; j++)
				if (!matrix[i][j].equals(BigInteger.ZERO))
					return false;
		return true;
	}

	public void swapRows(int a, int b) {
		swapRows2(a, b);
		if (next != null)
			next.swapColumns2(a, b);
		if (target != null) {
			int tmp = target.get(a);
			target.set(a, target.get(b));
			target.set(b, tmp);
		}
	}

	private void swapRows2(int a, int b) {
		BigInteger tmp[] = matrix[a];
		matrix[a] = matrix[b];
		matrix[b] = tmp;
	}

	public void swapColumns(int a, int b) {
		swapColumns2(a, b);
		if (prev != null)
			prev.swapRows2(a, b);
		if (source != null) {
			int tmp = source.get(a);
			source.set(a, source.get(b));
			source.set(b, tmp);
		}
	}

	private void swapColumns2(int a, int b) {
		for (int i = 0; i < rows; i++) {
			BigInteger tmp = matrix[i][a];
			matrix[i][a] = matrix[i][b];
			matrix[i][b] = tmp;
		}
	}

	public void addRow(int a, int b, BigInteger n) {
		addRow2(a, b, n);
		/*
		 * if (next != null) --- not needed for our purposes next.addColumn2(a,
		 * b, n);
		 */
		// assert target == null || target.get(a) == target.get(b);
	}

	private void addRow2(int a, int b, BigInteger n) { // a += b * n
		for (int i = 0; i < columns; i++)
			matrix[a][i] = matrix[a][i].add(matrix[b][i].multiply(n));
	}

	public void addColumn(int a, int b, BigInteger n) {
		// assert source == null || source.get(a) == source.get(b);
		addColumn2(a, b, n);
		/*
		 * if (prev != null) --- same as above prev.addRow2(a, b, n);
		 */
	}

	private void addColumn2(int a, int b, BigInteger n) { // a += b * n
		for (int i = 0; i < rows; i++)
			matrix[i][a] = matrix[i][a].add(matrix[i][b].multiply(n));
	}

	public void multRow(int a, BigInteger n) {
		multRow2(a, n);
		/*
		 * if (next != null) next.multColumn2(a, n);
		 */
	}

	private void multRow2(int a, BigInteger n) { // a *= n
		for (int i = 0; i < columns; i++)
			matrix[a][i] = matrix[a][i].multiply(n);
	}

	public void multColumn(int a, BigInteger n) {
		multColumn2(a, n);
		/*
		 * if (prev != null) prev.multRow2(a, n);
		 */
	}

	private void multColumn2(int a, BigInteger n) { // a *= n
		for (int i = 0; i < rows; i++)
			matrix[i][a] = matrix[i][a].multiply(n);
	}

	public int rowNonZeros(int i) {
		int ret = 0;
		for (int j = 0; j < columns; j++)
			if (!matrix[i][j].equals(BigInteger.ZERO))
				ret++;
		return ret;
	}

	public int columnNonZeros(int i) {
		int ret = 0;
		for (int j = 0; j < rows; j++)
			if (!matrix[j][i].equals(BigInteger.ZERO))
				ret++;
		return ret;
	}

	public int zeroRowsToEnd() {
		int nzrows = rows;
		for (int i = 0; i < nzrows; i++)
			while (rowNonZeros(i) == 0 && i < nzrows)
				swapRows(i, --nzrows);
		return nzrows;
	}

	public int zeroColumnsToEnd() {
		int nzcols = columns;
		for (int i = 0; i < nzcols; i++)
			while (columnNonZeros(i) == 0 && i < nzcols)
				swapColumns(i, --nzcols);
		return nzcols;
	}

	public void toSmithForm() {
		// first send all zero rows, columns to the end
		// int nzrows = zeroRowsToEnd();
		// int nzcols = zeroColumnsToEnd(); // nz means "non zero"
		// int n = Math.min(nzrows, nzcols);
		// int n = Math.min(rows, columns);
		for (int row = 0, col = 0; row < rows && col < columns; row++, col++) {
			while (row < rows && rowNonZeros(row) == 0)
				row++;
			while (col < columns && columnNonZeros(col) == 0)
				col++;
			if (row >= rows || col >= columns)
				break;
			if (row > col) {
				swapRows(row, col);
				row = col;
			} else if (col > row) {
				swapColumns(row, col);
				col = row;
			}
			while (rowNonZeros(row) != 1 || columnNonZeros(col) != 1
					|| matrix[row][col].compareTo(BigInteger.ZERO) <= 0) {
				for (int j = row; j < rows; j++)
					if (matrix[j][col].compareTo(BigInteger.ZERO) < 0)
						multRow(j, BigInteger.valueOf(-1));
				while (columnNonZeros(col) != 1
						|| matrix[row][col].compareTo(BigInteger.ZERO) == 0) {
					// find the min
					BigInteger min = BigInteger.valueOf(-1);
					int idxmin = -1;
					for (int j = row; j < rows; j++)
						if ((matrix[j][col].compareTo(min) < 0 || min
								.equals(BigInteger.valueOf(-1)))
								&& matrix[j][col].compareTo(BigInteger.ZERO) > 0) {
							min = matrix[j][col];
							idxmin = j;
						}
					if (idxmin != row)
						swapRows(row, idxmin);
					// subtract the min as much as possible from the others
					for (int j = row + 1; j < rows; j++)
						if (!matrix[j][col].equals(BigInteger.ZERO))
							addRow(j, row, matrix[j][col].divide(min).negate());
				}
				// nzrows = zeroRowsToEnd();
				// nzcols = zeroColumnsToEnd();
				// n = Math.min(nzrows, nzcols);
				for (int j = col + 1; j < columns; j++)
					if (matrix[row][j].compareTo(BigInteger.ZERO) < 0)
						multColumn(j, BigInteger.valueOf(-1));
				while (rowNonZeros(row) != 1
						|| matrix[row][col].equals(BigInteger.ZERO)) {
					BigInteger min = BigInteger.valueOf(-1);
					int idxmin = -1;
					for (int j = col; j < columns; j++)
						if ((matrix[row][j].compareTo(min) < 0 || min
								.equals(BigInteger.valueOf(-1)))
								&& matrix[row][j].compareTo(BigInteger.ZERO) > 0) {
							min = matrix[row][j];
							idxmin = j;
						}
					if (idxmin != col)
						swapColumns(col, idxmin);
					for (int j = col + 1; j < columns; j++)
						if (!matrix[row][j].equals(BigInteger.ZERO))
							addColumn(j, col, matrix[row][j].divide(min)
									.negate());
				}
				// nzrows = zeroRowsToEnd();
				// nzcols = zeroColumnsToEnd();
				// n = Math.min(nzrows, nzcols);
			}
		}
		zeroRowsToEnd();
		zeroColumnsToEnd();
	}

	public String toString() {
		StringBuilder sb = new StringBuilder();
		for (int i = 0; i < rows; i++) {
			for (int j = 0; j < columns; j++) {
				sb.append(matrix[i][j] + " ");
				sb.append("\r\n");
			}
		}
		return sb.toString();
	}
}
