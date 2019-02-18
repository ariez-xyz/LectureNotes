package pape_sismanovic.model;

import pape_sismanovic.enums.EVENT_TYPE;
import pape_sismanovic.interfaces.Observer;
import pape_sismanovic.interfaces.Observable;

import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;

/**
 * Observable library managing a HashMap of Books.
 * Books can be accessed via ISBN only. Iteration over ISBNs is possible.
 */
public class Library implements Iterable<String>, Observable {
    private HashMap<String, Book> library = new HashMap<>();
    private LinkedList<Observer> observers = new LinkedList<>();

    /**
     * Add a new book to the library.
     * Attempts to add multiple books with the same ISBN are ignored.
     * @param book Book instance to be added
     * @return Previously added Book with the same ISBN if already in library, else null
     */
    public Book add(Book book) {
        Book ret = library.putIfAbsent(book.getIsbn(), book);
        if (ret == null) // if book was actually inserted
            notifyObservers(new Event(EVENT_TYPE.ADD, book.getIsbn()));
        return ret;
    }

    /**
     * Deletes book with given ISBN (unless unknown)
     * @param isbn The ISBN
     * @return The book, or null if not present
     */
    public Book delete(String isbn) {
        Book ret = library.remove(isbn);
        if (ret != null)
            notifyObservers(new Event(EVENT_TYPE.DELETE, isbn));
        return ret;
    }

    public Book delete(Book book) {
        return delete(book.getIsbn());
    }

    /**
     * Edit (replace) book with given ISBN
     * @param isbn
     * @param updated
     * @return Old book or null if ISBN unknown
     */
    public Book edit(String isbn, Book updated) {
        if(library.get(isbn) != null) {
            Book ret = library.put(isbn, updated);
            notifyObservers(new Event(EVENT_TYPE.EDIT, isbn, updated.getIsbn()));
            return ret;
        }

        return null;
    }

    public Book edit(Book book, Book updated) {
        return edit(book.getIsbn(), updated);
    }

    /**
     * Access a book via ISBN.
     * @param isbn The ISBN
     * @return The book
     */
    public Book getBook(String isbn) {
        return library.get(isbn);
    }

    /**
     * @return Iterator over ISBNs
     */
    @Override
    public Iterator<String> iterator() {
        return library.keySet().iterator();
    }

    @Override
    public boolean registerObserver(Observer o) {
        return !observers.contains(o) && observers.add(o);
    }

    @Override
    public boolean removeObserver(Observer o) {
        return observers.remove(o);
    }

    @Override
    public void notifyObservers(Event e) {
        for (Observer o : observers)
            o.update(e);
    }
}

