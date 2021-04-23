//David Pape 01634454
//Johannes Spilka 11724817
//Filip Vecek 11700962

import java.util.ArrayList;

public class StackArray<E> extends Stack<E> {
    private ArrayList<E> array = new ArrayList<E>(10);

    public StackArray() {
    }

    public StackArray(E[] objects) {
        for(E object : objects)
            push(object);
    }

    public void push(E item) {
        array.add(item);
    }

    public E pop() {
        E object = array.get(array.size() - 1);
        array.remove(array.size() - 1);
        return object;
    }

    public E peek() {
        return array.get(array.size() - 1);
    }

    public boolean isEmpty() {
        return array.isEmpty();
    }

    public int contains(E item) {
        return array.indexOf(item);
    }

    public String toString() {
        if(array.isEmpty())
            return "[]";

        String s = "[";
        for (int i = array.size() - 1; i > 0; i--) {
            s += array.get(i) + ", ";
        }
        return s + array.get(0) + "]";
    }
}
