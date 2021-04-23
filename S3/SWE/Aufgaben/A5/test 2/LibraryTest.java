import pape_sismanovic.ItemNotFoundException;
import pape_sismanovic.Library;

import java.io.File;
import java.util.Scanner;

/**
 * Tester class for src/Library
 */
public class LibraryTest {
    public static void main(String[] args) throws Exception {
        Library lib = new Library();
        Scanner sc = new Scanner(System.in);
        String input = args.length > 0 ? args[0] : "input.xml";

        System.out.print("Reading file " + input + "... ");
        lib.loadXml(new File(input));
        System.out.println("done.");

        while(true) {
            String item = "";
            System.out.print("Find price of item: ");
            try {
                item = sc.nextLine();
                System.out.println(lib.getPrice(item));
            } catch (ItemNotFoundException e) {
                System.out.println("[ERROR] No item named \"" + item  + "\" could be found in " + input);
            }
        }
    }
}
