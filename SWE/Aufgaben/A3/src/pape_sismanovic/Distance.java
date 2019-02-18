package pape_sismanovic;

import assignment3_int.StringComparer;

/**
 * Offers a single method that returns the length difference of two input strings.
 */
public class Distance implements StringComparer {

    /**
     * Return (always positive) length difference of input strings s and t
     * @param s First input string
     * @param t Second input string
     * @return Length difference of s and t
     */
    public int compare(String s, String t) {
        return Math.abs(s.length() - t.length());
    }
}
