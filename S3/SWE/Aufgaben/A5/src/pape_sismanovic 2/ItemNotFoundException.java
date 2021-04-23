package pape_sismanovic;

/**
 * If no item with a specified name can be found in a Component object
 */
public class ItemNotFoundException extends RuntimeException {
    public ItemNotFoundException(String message) {
        super(message);
    }
}
