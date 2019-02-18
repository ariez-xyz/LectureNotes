import pape_sismanovic.Distance;
import pape_sismanovic.DistanceCalculator;
import pape_sismanovic.HammingDistance;
import pape_sismanovic.LevenshteinDistance;

/**
 * Tester class built for DistanceCalculator
 * Tests distances given by the three implemented StringComparer classes
 */
public class Tester {

    public static void main(String[] args) {
        DistanceCalculator c = new DistanceCalculator();
        String s = "Saturday";
        String t = "Sunday";

        c.setStringComparison(new LevenshteinDistance());
        System.out.println("Levenshtein: " + c.getDistance(s, t));

        c.setStringComparison(new HammingDistance());
        System.out.println("Hamming: " + c.getDistance(s, t));

        c.setStringComparison(new Distance());
        System.out.println("Distance: " + c.getDistance(s, t));
    }
}
