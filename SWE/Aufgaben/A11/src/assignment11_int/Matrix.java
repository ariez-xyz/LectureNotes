package assignment11_int;

import java.util.Arrays;
import java.util.function.BinaryOperator;
import java.util.function.Function;
import java.util.function.Supplier;

/**
 * A Rows-x-Cols matrix of doubles
 * 
 * @author andreas nad.
 *
 */
public class Matrix {
	private final int nofRows;
	private final int nofCols;
	private final double[][] data;
	
	public interface PrimitiveDoubleFunction {
		double apply(double x);
	}
	
	interface PrimitiveDoubleBinaryOp {
		double apply(double x, double y);
	}

	public Matrix(int nofRows, int nofCols, Supplier<Double> init) {
		this.nofRows = nofRows;
		this.nofCols = nofCols;
		this.data = new double[nofRows][nofCols];
		if (init != null) {
			for (int i = 0; i < nofRows; i++) {
				for (int j = 0; j < nofCols; j++) {
					data[i][j] = init.get();
				}
			}
		}
	}

	public Matrix(int nofRows, int nofCols) {
		this(nofRows, nofCols, null);
	}

	public Matrix(double[] data) {
		this(1, data.length, null);
		this.data[0] = data;
	}
	
	public Matrix(int nofRows, int nofCols, double constantInit) {
		this(nofRows, nofCols);
		for (int i=0;i<nofRows;i++) {
			Arrays.fill(this.data[i], constantInit);
		}
	}

	public int getNofRows() {
		return nofRows;
	}

	public int getNofCols() {
		return nofCols;
	}

	/** returns a new matrix which is the transpose of this matrix */
	public Matrix getTranspose() {
		Matrix m = new Matrix(nofCols, nofRows);
		for (int i = 0; i < nofRows; i++)
			for (int j = 0; j < nofCols; j++)
				m.data[j][i] = this.data[i][j];
		return m;
	}

	public Matrix getDotProduct(Matrix m) {
		if (nofCols != m.nofRows)
			throw new RuntimeException("illegal dimensions: " + getShape() + " . " + m.getShape());
		Matrix dot = new Matrix(this.getNofRows(), m.getNofCols());
		for (int i = 0; i < this.getNofRows(); i++) {
			for (int j = 0; j < m.getNofCols(); j++) {
				for (int k = 0; k < this.getNofCols(); k++) {
					dot.data[i][j] += data[i][k] * m.data[k][j];
				}
			}
		}
		return dot;
	}
	
	/* performs a little bit faster than Function<Double, Double> (avoids boxing/unboxing) */
	public Matrix apply(PrimitiveDoubleFunction f) {
		Matrix m = new Matrix(nofRows, nofCols);
		for (int i = 0; i < nofRows; i++) {
			for (int j = 0; j < nofCols; j++) {
				m.data[i][j] = f.apply(data[i][j]);
			}
		}
		return m;
	}

	public Matrix mult(double d) {
		Matrix m = new Matrix(nofRows, nofCols);
		for (int i = 0; i < nofRows; i++) {
			for (int j = 0; j < nofCols; j++) {
				m.data[i][j] = data[i][j] * d;
			}
		}
		return m;
	}

	public int getMaxColIdx() {
		if (nofCols > 1)
			throw new RuntimeException("not allowed for nofCols != 1");
		int maxIdx = 0;
		for (int i = 0; i < nofRows; i++) {
			if (data[i][0] > data[maxIdx][0]) {
				maxIdx = i;
			}
		}
		return maxIdx;
	}
	
	private Matrix op(PrimitiveDoubleBinaryOp op, Matrix o) {
		if (nofRows != o.nofRows || nofCols != o.nofCols)
			throw new RuntimeException("illegal dimensions: " + getShape() + " , " + o.getShape());
		Matrix m = new Matrix(nofRows, nofCols);
		for (int i = 0; i < nofRows; i++) {
			for (int j = 0; j < nofCols; j++) {
				m.data[i][j] = op.apply(data[i][j], o.data[i][j]);
			}
		}
		return m;
	}

	public Matrix mult(Matrix o) {
		return op((x, y) -> x * y, o);
	}

	// applies directly to this matrix!
	// adapted for performance (~20x speedup)
	public void plus(Matrix o) {
		if (nofRows != o.nofRows || nofCols != o.nofCols)
			throw new RuntimeException("illegal dimensions: " + getShape() + " , " + o.getShape());
		for (int i = 0; i < nofRows; i++) {
			for (int j = 0; j < nofCols; j++) {
				data[i][j] += o.data[i][j];
			}
		}
	}

	public Matrix minus(Matrix o) {
		return op((x, y) -> x - y, o);
	}

	public String getShape() {
		return "(" + nofRows + "," + nofCols + ")";
	}

	public String toString() {
		StringBuilder b = new StringBuilder("[");
		for (int i = 0; i < data.length; i++) {
			b.append(Arrays.toString(data[i]));
			if (i < data.length - 1) {
				b.append('\n');
			}
		}
		b.append("]");
		return b.toString();
	}
	
}
