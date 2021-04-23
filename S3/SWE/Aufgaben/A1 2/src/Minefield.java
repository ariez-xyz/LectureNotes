public class Minefield {
    boolean[][] mines;
    int n;
    int m;

    public Minefield(int n, int m) {
        this.n = n;
        this.m = m;
        this.mines = new boolean[n][m];
    }

    public void fillRow (int row, String input) throws InputFormatException {
        char[] chars = input.toCharArray();

        if (chars.length != m) throw new InputFormatException("unexpected line length near \"" + input + "\": expected " + m + " symbols");

        for(int i = 0; i < chars.length; i++)
            if (chars[i] == '*') mines[row][i] = true;
            else if (chars[i] == '.') mines[row][i] = false;
            else throw new InputFormatException("unexpected symbol: " + chars[i]);
    }

    public boolean hasBombAt(int n, int m) {
        return mines[n][m];
    }

    public String toString() {
        StringBuilder sb = new StringBuilder();

        sb.append(n + " " + m + "\n");
        for(boolean[] row : mines) {
            for(boolean b : row)
                if (b) sb.append("*");
                else sb.append(".");
            sb.append("\n");
        }

        return sb.toString();
    }
}
