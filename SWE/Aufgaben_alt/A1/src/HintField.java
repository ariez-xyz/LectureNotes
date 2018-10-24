public class HintField {
    int[][] field;
    Minefield corresponding;
    int n;
    int m;
    int hintsSum = 0;

    public HintField(int n, int m, Minefield correspondingMinefield) {
        this.n = n;
        this.m = m;
        this.field = new int[n][m];
        this.corresponding = correspondingMinefield;

        for (int i = 0; i < n; i++)
            for (int j = 0; j < m; j++)
                if (correspondingMinefield.hasBombAt(i, j)) field[i][j] = -1;
    }

    public boolean increase(int n, int m) {
        if (!corresponding.hasBombAt(n, m)) {
            field[n][m]++;
            return true;
        }
        return false;
    }

    public String toString() {
        StringBuilder sb = new StringBuilder();

        for(int[] row : field) {
            for(int i : row)
                if (i < 0) sb.append("*");
                else sb.append(i);
            sb.append("\n");
        }

        return sb.toString();
    }
}
