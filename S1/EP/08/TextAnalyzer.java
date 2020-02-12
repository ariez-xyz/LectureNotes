// David Pape 01634454
public class TextAnalyzer {
  int[] stats = new int[123], pairStats = new int[123];

  public void analyzeChar(String s) {
    for(char c : s.toLowerCase().toCharArray())
      if(c >= 'a')
        stats[c]++;
  }

  public int getFrequencyOfChar(char c) {
    return stats[c];
  }

  public void resetStatistic() {
    stats = new int[123];
    pairStats = new int[123];
  }

  public void printStatisticChar(char[] cs) {
    System.out.println("Char frequencies");
    for(char c : cs)
      System.out.println(c + ": " + stats[c]);
  }

  public void analyzeCharDoubles(String s) {
    char[] c = s.toLowerCase().toCharArray();
    for(int i = 1; i < c.length; i++)
      if(c[i] >= 'a' && c[i] == c[i - 1])
        pairStats[c[i]]++;
  }

  public int getFrequencyOfCharDouble(char c) {
    return pairStats[c];
  }

  public void printStatisticCharDoubles(char[] cs) {
    System.out.println("Char doubles frequencies");
    for(char c : cs)
      System.out.println(c + "" + c + ": " + pairStats[c]);
  }
}