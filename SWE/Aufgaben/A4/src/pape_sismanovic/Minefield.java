package pape_sismanovic;

import java.util.Arrays;

/**
 * Represents a minefield via a two-dimensional integer array.
 * Maximum (parseable) minepower is 9.
 */
public class Minefield {
    private int[][] field;
    int n;
    int m;

    /**
     * Create a minefield from input following the given format
     * Throws InputFormatException on bad input.
     * @param data line-wise input
     */
    public Minefield(String[] data) {
        String[] dimensions = data[0].split(" ");
        try {
            this.n = Integer.parseInt(dimensions[0]);
            this.m = Integer.parseInt(dimensions[1]);
        } catch (NumberFormatException e) {
            throw new InputFormatException("unexpected symbol in field header: \"" + data[0] + "\"");
        }

        if(data.length != n + 1)
            throw new InputFormatException("expected " + n + " rows, given " + data.length + ": " + Arrays.toString(data));
        field = new int[n][m];

        for(int i = 1; i <= n; i++) {
            if(data[i].length() != m)
                throw new InputFormatException("expected " + m + " symbols, given " + data[i].length() + ": \"" + data[i] + "\"");

            for(int j = 0; j < m; j++)
                field[i - 1][j] = parse(data[i].charAt(j));
        }
    }

    /**
     * Get power of mine at a given position
     * 0 if no mine is present
     * @param i row
     * @param j column
     * @return power of mine
     */
    public int get(int i, int j) {
        try {
            return field[i][j];
        } catch (ArrayIndexOutOfBoundsException e) {
            throw new PositionOutOfBoundsException(String.format("cannot access position (%d, %d) in %dx%d field", i, j, n, m));
        }
    }

    /**
     * Return true if position at specified coordinates is a mine
     * Equal to get(i, j) != 0
     * @param i row
     * @param j column
     * @return true if mine, false else
     */
    public boolean isMine(int i, int j) {
        try {
            return field[i][j] > 0;
        } catch (ArrayIndexOutOfBoundsException e) {
            throw new PositionOutOfBoundsException(String.format("cannot access position (%d, %d) in %dx%d field", i, j, n, m));
        }
    }

    private int parse(char c) {
        switch(c) {
            case '.': return 0;
            case '1': return 1; // pointless?
            case '*': return 2;
            case '2': return 2;
            case '3': return 3;
            case '4': return 4;
            case '5': return 5;
            case '6': return 6;
            case '7': return 7;
            case '8': return 8;
            case '9': return 9;
            default:  throw new InputFormatException("unexpected symbol: " + c);
        }
    }
}
