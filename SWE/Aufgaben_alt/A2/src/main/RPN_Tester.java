package main;

import exceptions.IllegalInputException;
import exceptions.SizeLimitReachedException;
import stacks.ArrayStack;
import stacks.LinkedListStack;
import java.util.Arrays;

/**
 * Simple tester class for RPN_Calculator that takes commandline arguments as input for the calculator
 * Defaults to some test input if no arguments are given. Default result should be -3.5
 * @author David Pape
 * @author Michael Sismanovic
 */
public class RPN_Tester {

    /**
     * Main method testing RPN_Calculator with a LinkedListStack and an ArrayStack.
     * Defaults to some test input if no arguments are given. Default result should be -3.5
     * @param args Optional string array of individual RPN input symbols
     * @throws IllegalInputException If input is not in proper RPN form
     * @throws SizeLimitReachedException If array-based stack is too small to handle input
     */
    public static void main(String[] args) throws IllegalInputException, SizeLimitReachedException {
        RPN_Calculator c = new RPN_Calculator();
        String[] defaultTestInput = {"1.5", "2", "+", "3", "4", "-", "*"};

        if(args.length == 0) {
            System.out.println("No input given... proceeding with default test input: " + Arrays.toString(defaultTestInput));
            args = defaultTestInput;
        }

        c.setStack(new LinkedListStack<Double>());
        System.out.println("LinkedList result:\t" + c.calc(args));

        c.setStack(new ArrayStack<Double>());
        System.out.println("ArrayStack result:\t" + c.calc(args));
    }

}
