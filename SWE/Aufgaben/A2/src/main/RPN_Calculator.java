package main;

import assignment2_int.Calculator;
import assignment2_int.Stack;
import exceptions.EmptyStackException;
import exceptions.IllegalInputException;
import exceptions.SizeLimitReachedException;
import exceptions.StackNotSetException;
import stacks.ArrayStack;

/**
 * Implements a calculator for mathematical expressions in reverse Polish notation.
 * Utilizes a stack that can be assigned to objects of this class via setStack()
 * However on instantiation no default stack is set. Hence setStack() must be called at least once before calc()
 * @author David Pape
 * @author Michael Sismanovic
 */
public class RPN_Calculator implements Calculator {

    private Stack<Double> stack;

    /**
     * Set the stack to be used. Must implement the assignment2_int.Stack interface
     * Must be called at least once before calc()
     * @param s The stack
     */
    public void setStack(Stack<Double> s) {
        stack = s;
    }

    /**
     * Takes a mathematical expression in reverse Polish notation as input and calculates the result.
     * @param input String array holding the individual symbols of the RPN expression.
     * @return The result
     * @throws IllegalInputException If the input is not in proper RPN notation.
     */
    @Override
    public double calc(String[] input) throws IllegalInputException, SizeLimitReachedException {
        if(stack == null)
            throw new StackNotSetException("a Stack<Double> passed via setStack() is required");

        for(String s : input)
            processInputSymbol(s);

        if(stack.size() != 1)
            throw new IllegalInputException("input doesn't give a sensible result");

        return stack.pop();
    }

    private void processInputSymbol(String s) throws IllegalInputException, SizeLimitReachedException {
        double operand2;    // for noncommutative operations, since the first stack element is supposed to be "on the right" of the operator, we cache it in this variable

        try {
            switch(s) {
                case "*":
                    stack.push(stack.pop() * stack.pop());
                    break;
                case "/":
                    operand2 = stack.pop();
                    stack.push(stack.pop() / operand2);
                    break;
                case "+":
                    stack.push(stack.pop() + stack.pop());
                    break;
                case "-":
                    operand2 = stack.pop();
                    stack.push(stack.pop() - operand2);
                    break;
                default:
                    try {
                        stack.push(Double.parseDouble(s));
                    } catch (NumberFormatException e) {
                        throw new IllegalInputException("unrecognized input symbol");
                    }
            }
        } catch (EmptyStackException e) {
            throw new IllegalInputException("attempted operation '" + s + "' on 0- or 1-element stack");
        } catch (SizeLimitReachedException e) {
            throw new SizeLimitReachedException("input too big - arraystack cannot hold more than " + ArrayStack.ARRAY_SIZE + " operands simultaneously");
        }
    }
}
