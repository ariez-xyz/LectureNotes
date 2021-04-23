package pape_sismanovic;

/**
 * Representation of a book with name, price and ISBN
 */
public class Book extends Component {
    private int isbn;
    private int price;

    /**
     * Instantiate new book with given price, name, ISBN
     */
    public Book(String name, int price, int isbn) {
        super(name);
        this.price = price;
        this.isbn = isbn;
    }

    /**
     * @return Price set at instantiation
     */
    @Override
    public int getPrice() {
        return price;
    }

    /**
     * @return ISBN set at instantiation
     */
    public int getIsbn() {
        return isbn;
    }
}
