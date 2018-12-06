package pape_sismanovic.exceptions;

/**
 * For bad input strings
 */
public class InputFormatException extends RuntimeException {
    public InputFormatException(String message) {
        super(message);
    }
}
