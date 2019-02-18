package pape_sismanovic;

import assignment3_int.Assignment3;
import assignment3_int.StringComparer;

/**
 * Calculates distance of two input strings as defined by a given StringComparer
 */
public class DistanceCalculator implements Assignment3{

    private StringComparer comparer = null;

    /**
     * Create new DistanceCalculator without a StringComparer
     * Needs to be set before first call of getDistance()
     */
    public DistanceCalculator() {

    }

    /**
     * Create new DistanceCalculator and set a StringComparer immediately
     * @param c StringComparer to be used
     */
    public DistanceCalculator(StringComparer c){
        comparer = c;
    }

    /**
     * Calculate distance of two given strings as defined by currently set StringComparer
     * @param s First input string
     * @param t Second input string
     * @return Resulting distance
     */
    @Override
    public int getDistance(String s, String t) {
        if(comparer == null)
            throw new UndefinedStringComparerException("required pass of a StringComparer via setStringComparison()");

        return comparer.compare(s, t);
    }

    /**
     * Set StringComparer to be used
     * @param c StringComparer instance to be used
     */
    @Override
    public void setStringComparison(StringComparer c) {
        comparer = c;
    }
}
