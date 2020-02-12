package pape_sismanovic;

import interfaces.Assignment1;
import interfaces.Assignment4;

import java.io.File;

/**
 * Class implementing minesweep() and minepower() as given in the corresponding assignments
 */
public class MSHint implements Assignment1, Assignment4 {

    /**
     * Print hints corresponding to minefields in a given inputfile to stdout
     * Mines are replaced with "*" for differentiability. Mine power is lost however and needs to be looked up in input.
     * @param f File object pointing to input file
     * @return Sum of all hints of all fields
     */
    public long minesweep (File f) {
        InputFile input = new InputFile(f);

        int count = 1;
        for(Minefield field : input)  {
            System.out.println("Field: " + count);

            for(int i = 0; i < field.n; i++) {
                StringBuilder line = new StringBuilder();
                for(int j = 0; j < field.m; j++)
                    line.append(field.isMine(i, j) ? "*" : minepower(field, i, j));
                System.out.println(line);
            }
            count++;
        }

        return getHintSum(input);
    }

    private long getHintSum(InputFile file) {
        long sum = 0;

        for(Minefield f : file)
            for (int i = 0; i < f.n; i++)
                for (int j = 0; j < f.m; j++)
                    if(!f.isMine(i, j))
                        sum += minepower(f, i, j);

        return sum;
    }

    /**
     * Returns minepower of position at coordinates (row, column) as defined in the assignment
     * @param field The Minefield
     * @param row Row of position
     * @param column Column of position
     * @return Sum of "forces" exerted by mines surrounding the field.
     */
    public int minepower(Minefield field, int row, int column) {
        int totalEffects = 0;

        if(row < 0 || row >= field.n || column < 0 || column >= field.m)
            throw new PositionOutOfBoundsException(String.format("cannot access position (%d, %d) in %dx%d field", row, column, field.n, field.m));

        for (int i = 0; i < field.n; i++) {
            for (int j = 0; j < field.m; j++) {
                int range = field.get(i, j);
                int rowDistance = Math.abs(row - i);
                int colDistance = Math.abs(column - j);
                int effect = range - Math.max(rowDistance, colDistance);

                totalEffects += effect > 0 ? effect : 0;
            }
        }

        return totalEffects;
    }

    /**
     * Assumes inputfile only holds one field. Ignores any other possibly existent fields.
     * Returns minepower of position at coordinates (row, column) as defined in the assignment
     * @param inputfile File object pointing to input file
     * @param row Row of position
     * @param column Column of position
     * @return Sum of "forces" exerted by mines surrounding the field.
     */
    @Override
    public int minepower(File inputfile, int row, int column) {
        return minepower(new InputFile(inputfile).getMinefield(0), row, column);
    }
}
