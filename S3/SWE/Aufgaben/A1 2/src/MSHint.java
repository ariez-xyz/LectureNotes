import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;

public class MSHint implements Assignment1 {

    public long minesweep (File inputfile) {
        ArrayList<Minefield> minefields = new ArrayList<>();
        int outputSum = 0;

        //read input
        try {
            minefields = readFile(inputfile);

            ArrayList<HintField> hintFields = new ArrayList<>();

            for(Minefield mf : minefields)
                hintFields.add(constructHintField(mf));

            for (int i = 0; i < hintFields.size(); i++) {
                System.out.println("Minefield: " + (i + 1));
                System.out.print(hintFields.get(i));
                outputSum += hintFields.get(i).hintsSum;
            }

            System.out.println(outputSum);
            return outputSum;

        } catch (InputFormatException e) {
            e.printStackTrace();

            return -1;
        }
    }

    private ArrayList<Minefield> readFile (File input) throws InputFormatException {
        ArrayList<Minefield> fields = new ArrayList<>();

        try (BufferedReader br = new BufferedReader(new FileReader(input))) {

            String line = br.readLine();
            if (line == null) throw new InputFormatException("empty input file");

            Minefield currentMinefield = fieldFromLine(line);

            while (true) {
                for (int i = 0; i < currentMinefield.n; i++) {
                    line = br.readLine();
                    if(line == null) throw new InputFormatException("unexpected EOF");

                    currentMinefield.fillRow(i, line);
                }

                line = br.readLine();
                if (line == null) break;
                fields.add(currentMinefield);
                currentMinefield = fieldFromLine(line);
            }

        } catch (IOException e) {
            e.printStackTrace();
        }

        return fields;
    }



    private Minefield fieldFromLine(String s) throws InputFormatException {
        String[] dimensions = s.split(" ");
        if (dimensions.length != 2) throw new InputFormatException("invalid line: \"" + s + "\", expected field dimensions.");

        int n = Integer.parseInt(dimensions[0]);
        int m = Integer.parseInt(dimensions[1]);

        return new Minefield(n, m);
    }

    private int increaseSurroundingFields(int row, int col, HintField HintField) {
        int increasedFields  = 0;

        for (int rowOffset = -1; rowOffset <= 1; rowOffset++)
            for (int colOffset = -1; colOffset <= 1; colOffset++) {

                int newRow = row + rowOffset;
                int newCol = col + colOffset;

                if ( 0 <= newRow
                  && newRow < HintField.n
                  && 0 <= newCol
                  && newCol < HintField.m
                  && HintField.increase(newRow, newCol))

                        increasedFields++;
            }

        return increasedFields;
    }

    private HintField constructHintField(Minefield minefield) {
        HintField hintField = new HintField(minefield.n, minefield.m, minefield);

        for (int row = 0; row < hintField.n; row++)
            for (int col = 0; col < hintField.m; col++) {

                if (minefield.hasBombAt(row, col)) {
                    hintField.hintsSum += increaseSurroundingFields(row, col, hintField);
                }
            }

        return hintField;
    }

}
