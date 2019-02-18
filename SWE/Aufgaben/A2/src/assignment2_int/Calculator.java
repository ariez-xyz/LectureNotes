package assignment2_int;

public interface Calculator {
    /** returns the result or throws an (Runtime-) Exception if anything goes wrong (e.g. illegal input) */
    public double calc(String[] input) throws Exception;
}