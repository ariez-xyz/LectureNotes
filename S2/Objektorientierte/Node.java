//David Pape 01634454
//Johannes Spilka 11724817
//Filip Vecek 11700962

public class Node<E> {
    private Node<E> next = null;
    private E object;

    public Node(E object) {
        this.object = object;
    }

    public Node getNext() {
        return next;
    }

    public void setNext(Node next) {
        this.next = next;
    }

    public E getObject() {
        return object;
    }
}
