package exceptions;

public class SizeLimitReachedException extends Exception {
    public SizeLimitReachedException(String message) {
        super(message);
    }
}
