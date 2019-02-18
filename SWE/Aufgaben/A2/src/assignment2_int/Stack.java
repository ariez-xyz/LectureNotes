package assignment2_int;

import exceptions.EmptyStackException;
import exceptions.SizeLimitReachedException;

public interface Stack<E> {

    /**
     * Pop an element off the stack
     * @return The element on top of the stack
     * @throws EmptyStackException If stack is empty
     */
    E pop() throws EmptyStackException;

    /**
     * Push an element onto the stack
     * @param item The element to be pushed
     * @throws SizeLimitReachedException If the stack has a a limit to its size and reached that
     */
    void push(E item) throws SizeLimitReachedException;

    /**
     * Get number of elements currently in the stack
     * @return Number of elements.
     */
    int size();
}