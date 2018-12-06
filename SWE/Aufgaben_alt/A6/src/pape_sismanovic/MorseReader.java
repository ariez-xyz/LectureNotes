package pape_sismanovic;

import java.io.*;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Set;

/**
 * Decorator class for java.io.Reader that is capable of reading morsecode.
 */
public class MorseReader extends Reader {

    private static final String DEFAULT_DECODE_TABLE = "morseData.csv";
    private HashMap<String, Character> morseTable; // maps morse encoded symbols to alphanumeric
    private Reader in;

    /**
     * Instantiate with specified decode table
     * @param reader Reader instance to be used
     * @param decodeTable File object pointing to decode table
     * @throws MorseException If decode table is bad
     * @throws IOException If decode table cannot be read
     */
    public MorseReader(Reader reader, File decodeTable) throws MorseException, IOException {
        morseTable = readDecodeTable(decodeTable);
        this.in = reader;
    }

    /**
     * Instantiate with default decode table
     * @param reader Reader instance to be used
     * @throws MorseException If decode table is bad
     * @throws IOException If decode table cannot be read
     */
    public MorseReader(Reader reader) throws MorseException, IOException {
        this(reader, new File(DEFAULT_DECODE_TABLE));
    }

    private HashMap<String, Character> readDecodeTable(File decodeTable) throws MorseException, IOException{
        HashMap<String, Character> table = new HashMap<>();

        try (BufferedReader br = new BufferedReader(new FileReader(decodeTable))) {
            String line;

            while((line = br.readLine()) != null) {
                if(!line.startsWith("//")) { // for comments
                    String[] code = line.split(",");

                    if (code[0].length() != 1) // make sure only chars are encoded
                        throw new MorseException(String.format("Illegal encoding: \"%s\"", line));

                    table.put(code[1], code[0].charAt(0));
                }
            }
        } catch (FileNotFoundException e) {
            throw new MorseException(String.format("Morse decode table \"%s\" not found", decodeTable.getName()));
        }

        table.put(" ", ' ');
        table.put("\n", ' ');
        return table;
    }

    /**
     * Reads morsecode.
     * Morse codewords (e.g. "--.") are counted as a single character
     * Single whitespaces are ignored, consecutive whitespaces are added
     * Input file must only contain symbols in {'-', '.', ' ', '\n'}
     * @param cbuf Buffer to write read characters to
     * @param off Starting index
     * @param len Maximum number of characters to read
     * @return Number of characters read or -1 if called at EOF
     * @throws IOException
     */
    @Override
    public int read(char[] cbuf, int off, int len) throws IOException {
        ArrayList<Integer> codeword;
        boolean readNewline = false;

        int readCount = 0;
        while (readCount < len) {

            if (readNewline) {
                cbuf[off + readCount++] = '\n';
                readNewline = false;
                continue;
            }

            codeword = readWord();

            if (codeword == null) // EOF
                return readCount > 0 ? readCount : -1;
            else if (codeword.size() == 0) // whitespace
                cbuf[off + readCount++] = ' ';
            else if (codeword.get(codeword.size() - 1) == '\n') { //newline
                codeword.remove(codeword.size() - 1);
                cbuf[off + readCount++] = parseCodeword(codeword);
                readNewline = true;
            } else // regular codeword
                cbuf[off + readCount++] = parseCodeword(codeword);
        }
        return readCount;
    }

    /**
     * read word (defined as {'-', '.'}^*{' ', '\n', EOF})
     */
    private ArrayList<Integer> readWord() throws IOException {
        ArrayList<Integer> word = new ArrayList<>();
        int currentChar = in.read();

        if (currentChar == -1)
            return null;

        while (currentChar == '-' || currentChar == '.') {
            word.add(currentChar);
            currentChar = in.read();
        }

        if (currentChar == '\n') {
            int help = '\n';
            word.add(help);
        }

        return word;
    }

    private Character parseCodeword (ArrayList<Integer> codeword) {
        StringBuilder sb = new StringBuilder();
        for (int c : codeword)
            sb.append((char) c);

        Character result = morseTable.get(sb.toString());

        if (result == null)
            throw new MorseException(String.format("Unknown codeword \"%s\"", sb.toString()));

        return result;
    }

    /**
     * Closes the reader specified at instantiation.
     * @throws IOException If reader throws IOException
     */
    @Override
    public void close() throws IOException {
        in.close();
    }

    /**
     * Following methods are simple wrappers passing calls to the Reader instance specified at instantiation
     * and modifying the return values such that morse codewords are decoded and treated as single characters.
     */

    public int read(java.nio.CharBuffer target) throws IOException {
        int len = target.remaining();
        char[] cbuf = new char[len];
        int n = read(cbuf, 0, len);
        if (n > 0)
            target.put(cbuf, 0, n);
        return n;
    }

   public int read() throws IOException {
        ArrayList<Integer> word = readWord();
        word.remove(word.size() - 1);
        return parseCodeword(word);
    }

   public int read(char cbuf[]) throws IOException {
        return read(cbuf, 0, cbuf.length);
    }

   public long skip(long n) throws IOException {
        long i;
        for (i = 0; i < n; i++) {
            ArrayList<Integer> word = readWord();
            if (word == null)
                break;
        }
        return i;
    }

   public boolean ready() throws IOException {
        return in.ready();
    }

   public boolean markSupported() {
        return in.markSupported();
    }

   public void mark(int readAheadLimit) throws IOException {
        in.mark(readAheadLimit);
    }

   public void reset() throws IOException {
        in.reset();
    }
}
