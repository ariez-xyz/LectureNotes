package pape_sismanovic;

import assignment2_int.Stack;
import exceptions.EmptyStackException;

/**
 * Stack implementation using a linked list
 * @param <E> The type of object this stack is supposed to hold
 * @author David Pape
 * @author Michael Sismanovic
 */
public class LinkedListStack<E> implements Stack<E> {

    private int size;
    private Node<E> head;

    /**
     * Construct an empty stack
     */
    public LinkedListStack(){
        size = 0;
        head = null;
    }

    /**
     * Push an element onto the stack
     * @param item The element to be pushed
     */
    @Override
    public void push(E item){
        Node<E> newNode = new Node<>(item);

        if(head != null)
            newNode.next = head;

        head = newNode;
        size++;
    }

    /**
     * Pop an element off the stack
     * @return The element on top of the stack
     * @throws EmptyStackException If stack is empty
     */
    @Override
    public E pop() throws EmptyStackException {
        if(head == null)
            throw new EmptyStackException("pop operation on empty stack");

        Node<E> n = head;
        head = head.next;
        size--;
        return n.item;
    }

    /**
     * Get number of elements currently in the stack
     * @return Number of elements.
     */
    @Override
    public int size(){
        return size;
    }

    @Override
    public E peek() throws EmptyStackException {
        if(head == null)
            throw new EmptyStackException("peek operation on empty stack");
        return head.item;
    }

    private class Node<E> {
        E item;
        Node<E> next;

        private Node(E item){
            this.item = item;
        }
    }
}