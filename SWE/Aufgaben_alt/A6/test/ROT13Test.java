import pape_sismanovic.ROT13Writer;

import java.io.BufferedWriter;
import java.io.FileWriter;

/**
 * Tester class for ROT13Writer. Writes a test string to out.txt
 */
public class ROT13Test {
    public static void main(String[] args) throws Exception {
        ROT13Writer w = new ROT13Writer(new BufferedWriter(new FileWriter("out.txt")));
        w.write("test string please ignore");
        w.flush();
        w.close();
    }
}
