package pape_sismanovic;

import assignment3_int.StringComparer;

/**
 * Offers a single method that returns the Hamming distance of two input strings.
 */
public class HammingDistance implements StringComparer {

    /**
     * Return Hamming distance of strings s and t
     * Here, differences in length raise the Hamming distance by 1 per char
     * @param s String to be compared
     * @param t String to be compared
     * @return Hamming distance.
     */
    public int compare(String s, String t) {
        int hammingCounter = Math.abs(s.length() - t.length());;
        int minLength = Math.min(s.length(), t.length());

        for (int i = 0; i < minLength; i++)
            if (s.charAt(i) != t.charAt(i))
                hammingCounter++;

        return hammingCounter;

    }
}
