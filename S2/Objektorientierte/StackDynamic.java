//David Pape 01634454
//Johannes Spilka 11724817
//Filip Vecek 11700962

public class StackDynamic<E> extends Stack<E> {
    Node<E> top;
    String typeName;

    public StackDynamic() {
    }
    public StackDynamic(E[] objects) {
        for (E object : objects)
            push(object);
    }

    public void push(E item) {
        Node<E> node = new Node<>(item);
        node.setNext(top);
        top = node;
    }

    public E pop() {
        if(top == null)
            return null;

        E object = top.getObject();
        top = top.getNext();
        return object;
    }

    public E peek() {
        if(top == null)
            return null;

        return top.getObject();
    }

    public boolean isEmpty() {
        return top == null;
    }

    public int contains(E item) {
        int index = 1;
        Node<E> current = top;

        for(;;)
            if (current == null)
                return -1;
            else if (current.getObject().equals(item))
                return index;
            else {
                current = current.getNext();
                index++;
            }
    }

    public String toString() {
        if (top == null)
            return "[]";

        String s = "[";
        Node<E> current = top;

        for(;;)
            if(current.getNext() == null)
                return s + current.getObject() + "]";
            else {
                s += current.getObject() + ", ";
                current = current.getNext();
            }
    }
}
