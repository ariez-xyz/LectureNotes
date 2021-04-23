package pape_sismanovic;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Iterator;

/**
 * Can parse a File and create a corresponding array of Minefields
 * Minefields are then accessible via a getter method and can also be iterated over
 */
public class InputFile implements Iterable<Minefield>{
    private Minefield[] fields;

    /**
     * Parses a file and creates corresponding Minefields
     * @param f Input File
     */
    public InputFile(File f) {
        ArrayList<Minefield> fieldsTemp = new ArrayList<>();

        try (BufferedReader br = new BufferedReader(new FileReader(f))) {

            while(true) { // iterates over fields in input
                String line = br.readLine();
                int currentFieldLen;

                try {
                    currentFieldLen = Integer.parseInt(line.split(" ")[0]);
                } catch (NullPointerException e) { // EOF
                    break;
                } catch (NumberFormatException e) { // bad input
                    throw new InputFormatException("unexpected symbol: " + line.split(" ")[0]);
                }

                if(currentFieldLen == 0)
                    continue;

                String[] currentFieldStrs = new String[currentFieldLen + 1];
                currentFieldStrs[0] = line;
                for(int i = 1; i <= currentFieldLen; i++) // iterates over lines
                    currentFieldStrs[i] = br.readLine();

                fieldsTemp.add(new Minefield(currentFieldStrs));
            }
        } catch (IOException e) {
            e.printStackTrace();
        }

        fields = new Minefield[fieldsTemp.size()];
        for (int i = 0; i < fieldsTemp.size(); i++)
            fields[i] = fieldsTemp.get(i);
    }

    /**
     *
     * @param index
     * @return
     */
    public Minefield getMinefield(int index) {
        return fields[index];
    }

    @Override
    public Iterator<Minefield> iterator() {
        return new Iterator<Minefield>() {
            int i = 0;

            @Override
            public boolean hasNext() {
                return i < fields.length;
            }

            @Override
            public Minefield next() {
                return fields[i++];
            }
        };
    }

    public boolean isSingleton() {
        return fields.length == 1;
    }
}
