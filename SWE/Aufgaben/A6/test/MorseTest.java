import pape_sismanovic.MorseReader;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.LineNumberReader;

/**
 * Tester class for MorseReader
 */
public class MorseTest {
    public static void main(String[] args) throws Exception {
        BufferedReader br = new BufferedReader(new MorseReader(new FileReader(new File("test.morse"))));
        System.out.println(br.readLine());
        System.out.println(br.readLine());
        System.out.println(br.readLine()); // null

        LineNumberReader lr = new LineNumberReader(new MorseReader(new BufferedReader(new FileReader(new File("test.morse")))));
        System.out.println(lr.readLine());
        System.out.println(lr.readLine());

        LineNumberReader ridiculousTest = new LineNumberReader(new LineNumberReader(new BufferedReader(new MorseReader(new BufferedReader(new BufferedReader(new FileReader("test.morse")))))));
        System.out.println(ridiculousTest.readLine());
    }
}
