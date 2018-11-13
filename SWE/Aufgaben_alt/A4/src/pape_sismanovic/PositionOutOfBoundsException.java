package pape_sismanovic;

/**
 * Similar to IndexOutOfBounds exception, thrown when an attempt to access a position outside a given minefield
 * is made.
 */
public class PositionOutOfBoundsException extends RuntimeException {
    public PositionOutOfBoundsException(String msg) {
        super(msg);
    }
}
