// David Pape 01634454
import java.util.ArrayList;

public class Cell {
  int value = 0;
  int size = 0;
  ArrayList<Integer> options = new ArrayList<Integer>();

  /** Constructs a cell for a Sudoku of given size.
  *
  * @param size           the size n of the Sudoku
  * @param value          the value of the cell, may be EMPTY
  *
  */
  public Cell (int size, int value) {
    this.size = size;
    this.value = value;
    for (int i = 1; i <= size; i++)
      options.add(i);
  }

  /** Returns the value of this cell.
  *
  * @return               the value
  *
  */
  public int getValue() {
    return value;
  }

  /** Indicates if cell is empty, i.e., has no value.
  *
  * @return               true, if empty
  *
  */
  public boolean isEmpty() {
    return value == 0;
  }

  /** Erases the given value from options and returns changes.
  *
  * @return               true, if erase action changed options
  *
  */
  public boolean erase(int value) {
    for (int i = 0; i < options.size(); i++) {
      if (options.get(i) == value && this.value == 0) {
        options.remove(i);
        return true;
      }
    }

    if (this.value == 0)
      return true;
    return false;
  }

  /** Returns the current number of possible values of this cell.
  *
  * @return               the number of possible values, 0, if the cell has a value
  *
  */
  public int getOptionCount() {
    if (value == 0)
      return options.size();
    return 0;
  }

  /** Checks options to give this cell a value and returns changes.
  *
  * @return               true, if  cell state changed from empty to not empty
  *
  */
  public boolean update() {
    if (getOptionCount() == 1) {
      value = options.get(0);
      return true;
    }

    return false;
  }
}
