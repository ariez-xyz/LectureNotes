package pape_sismanovic.model;

/**
 * Represents a book. Holds metadata about the book. Immutable.
 */
public class Book {
    private final String isbn;
    private final int year;
    private final String title;
    private final String[] authors;

    /**
     * @param isbn ISBN of this book
     * @param year Release year
     * @param title Title
     * @param authors Array of authors (can be length 1 of course)
     */
    public Book(String isbn, int year, String title, String[] authors) {
        this.isbn = isbn;
        this.year = year;
        this.title = title;
        this.authors = authors;
    }

    /**
     * Wrapper for convenience when adding books with one author
     */
    public Book(String isbn, int year, String title, String author) {
        this.isbn = isbn;
        this.year = year;
        this.title = title;
        this.authors = new String[]{author};
    }

    public String getIsbn() {
        return isbn;
    }

    public int getYear() {
        return year;
    }

    public String getTitle() {
        return title;
    }

    public String[] getAuthors() {
        return authors;
    }
}
