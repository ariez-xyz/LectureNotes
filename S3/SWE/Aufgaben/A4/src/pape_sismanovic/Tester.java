package pape_sismanovic;

import java.io.File;
import java.util.InputMismatchException;
import java.util.Scanner;

/**
 * Tester class for some Minesweeper-related classes
 */
public class Tester {
    private static Scanner sc = new Scanner(System.in);

    /**
     * Takes some user input and performs according operations on given Minefields
     * e.g. running minesweep() or minepower()
     * @param args Path to input file [optional]
     */
    public static void main(String[] args) {
        String path;

        if(args.length == 0) {
            System.out.print("Path to input file: ");
            path = sc.nextLine();
        }
        else
            path = args[0];


        File input = new File(path);
        if(!input.exists()) {
            printError("File " + path + " not found");
            return;
        }

        System.out.println("Parsing file: " + path);

        MSHint hint = new MSHint();
        InputFile in;
        try {
            in = new InputFile(new File(path));
        } catch (InputFormatException e) {
            printError("Bad input file: " + e.getMessage());
            return;
        }

        if (ask("Run minesweep() on input?")) {
            System.out.println(hint.minesweep(input));
        }

        while (ask("Run minepower() for given position?")) {
            int i = readInt("x-coordinate");
            int j = readInt("y-coordinate");
            int field = 1;

            if(!in.isSingleton())
                field = readInt("field #"); // 1 indexed since minesweep() output format is also 1 indexed.

            try {
                System.out.println("Result: " + hint.minepower(in.getMinefield(field - 1), i, j));
            } catch (PositionOutOfBoundsException e) {
                printError("Please enter a valid position: " + e.getMessage());
            } catch (ArrayIndexOutOfBoundsException e) {
                printError(field + " is not a valid field.");
            }
        }

        System.out.println("Exiting...");
    }

    private static boolean ask(String question) {
        while(true) {
            System.out.print(question + " [y/n]: ");
            String in = sc.nextLine();
            switch (in) {
                case "y": return true;
                case "n": return false;
            }
        }
    }

    private static void printError(String err) {
        System.out.println("[ERROR] " + err);
    }

    private static int readInt(String message) {
        while(true) {
            try {
                System.out.print(message + ": ");
                return sc.nextInt();
            } catch (InputMismatchException e) {
                System.out.println("please enter an integer...");
                sc.next();
            }
        }
    }
}
