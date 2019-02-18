public class ANNException extends RuntimeException {
    public ANNException(String msg) {
        super("[ERROR] " + msg);
    }
}
