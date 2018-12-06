package pape_sismanovic;

import java.io.IOException;
import java.io.Writer;
import java.util.HashMap;

/**
 * Decorator for java.io.Writer implementing simple ROT encoding (letters only)
 */
public class ROT13Writer extends Writer {

    private Writer out;
    private final int ROT;
    private HashMap<Character, Character> encodeTable;

    /**
     * Instantiate with default value for ROT encoding (13)
     * @param out Writer instance
     */
    public ROT13Writer(Writer out) {
        this(out, 13);
    }

    /**
     * Instantiate with specified value for ROT encoding
     * @param out Writer instance
     * @param ROT Value for ROT encoding
     */
    public ROT13Writer(Writer out, int ROT) {
        this.out = out;
        this.ROT = ROT % 26;
        encodeTable = generateEncodeTable();
    }

    private Character encodeCharacter(Character c) {
        Character ret = encodeTable.get(c);
        return ret == null ? c : ret;
    }

    private HashMap<Character, Character> generateEncodeTable() {
        HashMap<Character, Character> map = new HashMap<>();

        for(char c = 'A'; c <= 'Z'; c++) {
            char encoded = (char) (c + ROT);
            if (encoded > 'Z')
                encoded = (char) (c + (ROT - 26));
            map.put(c, encoded);
        }

        for(char c = 'a'; c <= 'z'; c++) {
            char encoded = (char) (c + ROT);
            if (encoded > 'z')
                encoded = (char) (c + (ROT - 26));
            map.put(c, encoded);
        }

        return map;
    }

    /**
     * Following methods should behave exactly the same as the ones in the Writer object passed at instantiation
     * except for "encrypting" output with given ROT encoding.
     */

    @Override
    public void write(char[] cbuf, int off, int len) throws IOException {
        for (int i = 0; i < len; i++)
            cbuf[off + i] = encodeCharacter(cbuf[off + i]);
        out.write(cbuf, off, len);
    }

    @Override
    public void flush() throws IOException {
        out.flush();
    }

    @Override
    public void close() throws IOException {
        out.close();
    }

    @Override
    public void write(int c) throws IOException {
        out.write(encodeCharacter((char) c));
    }

    @Override
   public void write(char cbuf[]) throws IOException {
        write(cbuf, 0, cbuf.length);
    }

    @Override
    public void write(String str) throws IOException {
        write(str, 0, str.length());
    }

    @Override
    public void write(String str, int off, int len) throws IOException {
        StringBuilder sb = new StringBuilder();
        for (Character c : str.toCharArray())
            sb.append(encodeCharacter(c));
        out.write(sb.toString(), off, len);
    }

    @Override
   public Writer append(CharSequence csq) throws IOException {
        return append(csq, 0, csq.length());
    }

    /**
     * Since char sequences are immutable, this encodes and appends char sequences character by character.
     * @param csq Char sequence to be written
     * @param start Starting index
     * @param end End index
     * @return this
     * @throws IOException If and I/O error occurs
     */
    @Override
   public Writer append(CharSequence csq, int start, int end) throws IOException {
        for (int i = start; i < end; i++)
            out.append(encodeCharacter(csq.charAt(i)));
        return this;
    }

    @Override
   public Writer append(char c) throws IOException {
        return out.append(encodeCharacter(c));
    }
}
