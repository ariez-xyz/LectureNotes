package pape_sismanovic;

import assignment3_int.StringComparer;

/**
 * Offers a single method that returns the Levenshtein distance of two input strings.
 */
public class LevenshteinDistance implements StringComparer {

    /**
     * Dynamic Programming algorithm to compute Levenshtein distance of two strings [Wagner-Fischer]
     * @param s First string to compare
     * @param t Second string to compare
     * @return Levenshtein distance of s and t
     */
    public int compare(String s, String t) {
        // distanceMatrix[i][j] holds the Levenshtein distance between the first i (j) characters of s (t)
        // i.e. the number of edits needed to get from s_i (source) to t_j (target)
        int[][] distanceMatrix = new int[s.length() + 1][t.length() + 1];

        // for 0-length source/target the distance is trivial
        for(int i = 0; i <= s.length(); i++)
            distanceMatrix[i][0] = i;
        for(int i = 0; i <= t.length(); i++)
            distanceMatrix[0][i] = i;

        // based on solutions to subproblems we calculate the full solution (dynamic programming)
        for(int i = 1; i <= s.length(); i++)
            for(int j = 1; j <= t.length(); j++) {

                int substituteCost = distanceMatrix[i-1][j-1];
                if(s.charAt(i - 1) != t.charAt(j - 1))
                    substituteCost++;

                int deleteCost = distanceMatrix[i - 1][j] + 1;
                int insertCost = distanceMatrix[i][j - 1] + 1;

                distanceMatrix[i][j] = min(substituteCost, insertCost, deleteCost);
            }

        return distanceMatrix[s.length()][t.length()];
    }

    private int min(int a, int b, int c){
        return Math.min(Math.min(a, b), c);
    }
}
