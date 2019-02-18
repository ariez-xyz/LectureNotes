package stacks;

import assignment2_int.Stack;
import exceptions.EmptyStackException;
import exceptions.SizeLimitReachedException;

/**
 * Stack implementation using an array of fixed size
 * Thus has a maximum number of elements it can hold.
 * @param <E> The type of object this stack is supposed to hold
 * @author David Pape
 * @author Michael Sismanovic
 */
public class ArrayStack <E> implements Stack<E> {

    /**
     * Size of the array that is used to hold elements for our stack
     */
    public static final int ARRAY_SIZE = 100;

    private Object[] stack;
    private int size;

    /**
     * Construct an empty stack with capacity equal to ARRAY_SIZE
     */
    public ArrayStack(){
        stack = new Object[ARRAY_SIZE];
        size = 0;
    }

    /**
     * Push an element onto the stack
     * @param item The element to be pushed
     * @throws SizeLimitReachedException If array is full
     */
    @Override
    public void push(E item) throws SizeLimitReachedException {
        if(size == stack.length)
            throw new SizeLimitReachedException("no space left in the array");

        stack[size] = item;
        size++;
    }

    /**
     * Pop an element off the stack
     * @return The element on top of the stack
     * @throws EmptyStackException If stack is empty
     */
    @Override
    @SuppressWarnings("unchecked")
    public E pop() throws EmptyStackException {
        if(size == 0)
            throw new EmptyStackException("pop operation on empty stack");

        E result = (E) stack[size - 1];

        size--;
        return result;
    }

    /**
     * Get number of elements currently in the stack
     * @return Number of elements.
     */
    @Override
    public int size(){
        return size;
    }
}
