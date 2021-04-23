// David Pape 01634454

public class Sudoku {
  Cell[][] cells;

  /** Constructs the Sudoku.
  *
  * @param values         the unsolved Sudoku (empty cells are 0)
  *
  */
  public Sudoku (int[][] values) {
    cells = new Cell[values.length][values[0].length];

    for (int i = 0; i < values.length; i++)
      for (int j = 0; j < values[i].length; j++)
        cells[i][j] = new Cell(values.length, values[i][j]);
  }

  /** Updates a logical part of the Sudoku by erasing known values and updating cells.
  *
  * @return               true, if any cell in the part changed (including cell options)
  *
  */
  public boolean update(Cell[] part) {
    for (Cell c : part)
      if (c.getValue() != 0)
        for (Cell d : part)
          d.erase(c.getValue());

    boolean change = false;

    for (Cell c : part)
      if(c.update())
        change = true;

    return change;
  }

  /** Returns the string representation of this Sudoku.
  *
  * @return               the Sudoku string
  *
  */
  public String toString() {
    String s = "";
    for (Cell[] ca : cells) {
      for (Cell c : ca)
        s += c.getValue() + " ";
      s += "\n";
    }

    return s;
  }

  /** Solves the Sudoku by updating rows, columns, and sub-grids
  * as long as there is no more change. */
  public void solve() {
    boolean change = true;

    while(change) {
      change = false;
      for(int i = 0; i < cells.length; i++) {
        Cell[] row = new Cell[cells.length];
        Cell[] segment = new Cell[cells.length];

        for (int k = 0; k < cells.length; k++)
          row[k] = cells[k][i];

        for (int k = 0; k < cells.length; k++)
          segment[k] = cells[k % (int) Math.sqrt(cells.length) + (i % (int) Math.sqrt(cells.length)) * (int) Math.sqrt(cells.length)][k / (int) Math.sqrt(cells.length) + (i / (int) Math.sqrt(cells.length)) * (int) Math.sqrt(cells.length)];

        if(update(cells[i]) || update(row) || update(segment))
          change = true;
      }
    }
  }
}
