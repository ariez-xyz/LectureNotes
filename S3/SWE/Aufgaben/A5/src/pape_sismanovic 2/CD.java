package pape_sismanovic;

/**
 * Representation of a CD with name and price
 */
public class CD extends Component {
    private int price;

    /**
     * Create new CD with given name and price
     */
    public CD(String name, int price) {
        super(name);
        this.price = price;
    }

    /**
     * @return Price set at instantiation
     */
    @Override
    public int getPrice() {
        return price;
    }
}
