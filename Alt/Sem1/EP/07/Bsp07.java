// David Pape 01634454

class Bsp07 {
  public static int[][] initBoard(int n) {
    if (n % 2 != 0) n++;
    int[][] a = new int[n][n];

    for (int i = 0; i < n; i++)
      for (int j = 0; j < n; j += 2) {
        a[i][j] = i * (n / 2) + (j / 2) + 1;
        a[i][j + 1] = i * (n / 2) + (j / 2) + 1;
      }

    return a;
  }

  public static int getToken(int[][] board, int row, int col) {
    try {
      return board[row][col];
    } catch (IndexOutOfBoundsException e){
      return -1;
    }
  }

  public static boolean switchPair(int[][] board, int row1, int col1, int row2, int col2) {
    try {
      int c = board[row1][col1];
      board[row1][col1] = board[row2][col2];
      board[row2][col2] = c;
      return true;
    } catch (IndexOutOfBoundsException e){
      return false;
    }
  }

  public static void shuffleBoard(int[][] board) {
    for (int i = 0; i < board.length; i++)
      for (int j = 0; j < board[i].length; j++)
        switchPair(board, i, j, PRNG.randomInt(board.length), PRNG.randomInt(board[i].length));
  }

  public static int[] findToken(int[][] board, int token, int tokenSequence) {
    for (int i = 0; i < board.length; i++)
      for (int j = 0; j < board[i].length; j++)
        if (board[i][j] == token) {
          if (tokenSequence == 1)
            return new int[]{i, j};
          else
            tokenSequence--;
        }

    return null;
  }

  public static int[][] solveBoard(int[][] board) {
    int numberOfTokens = (board.length / 2) * board.length;
    int[][] solution = new int[numberOfTokens][4];

    for (int i = 0; i < numberOfTokens; i++) {
      int[] tokenPos = findToken(board, i + 1, 1);
      solution[i][0] = tokenPos[0];
      solution[i][1] = tokenPos[1];

      tokenPos = findToken(board, i + 1, 2);
      solution[i][2] = tokenPos[0];
      solution[i][3] = tokenPos[1];
    }

    return solution;
  }
}
