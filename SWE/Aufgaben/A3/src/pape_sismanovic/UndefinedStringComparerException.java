package pape_sismanovic;

/**
 * Thrown if getDistance() is called on a DistanceCalculator instance before a StringComparer is assigned to it
 */
public class UndefinedStringComparerException extends RuntimeException {
    public UndefinedStringComparerException(String message) {
        super(message);
    }
}
