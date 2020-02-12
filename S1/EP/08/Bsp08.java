// David Pape 01634454

public class Bsp08 {
  public static void main(String[] args) {
    String text = "aabgedeogjsrlghjsilhjr"; // = SavitchIn.readLine();
    TextAnalyzer analyze = new TextAnalyzer();
    analyze.analyzeCharDoubles(text);

    for (char c = 'a'; c <= 'z'; c++)
      System.out.println(String.format("%1$c%1$c: %2$d", c, analyze.getFrequencyOfCharDouble(c)));
  }
}
