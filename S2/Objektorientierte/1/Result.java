//Filip Vecek     11700962
//Johannes Spilka 11724817
//David Pape      01634454

public class Result implements Measurable {
  double[] scores;
  String name;

  public Result(String name, int scores) {
    this.scores = new double[scores];
    this.name = name;
  }

  public double getMeasure() {
    return scores.length;
  }

  public double getScore(int which) {
    return scores[which];
  }

  public void setScore(int which, double amount) {
    scores[which] = amount;
  }

  //summiere alle Werte in scores
  public double getTotalScore() {
    double total = 0;

    for(double d : scores)
      total += d;

    return total;
  }

  //"stringifiziere" die Daten entspr. Vorgabe
  public String toString() {
    String s = name + ": ";

    //haenge alle Werte in scores zusammen mit " / " an s an
    for (double d : scores)
      s += d + " / ";

    //entferne das ueberfluessige " / " am Ende von s
    s = s.substring(0, s.length() - 2);

    //fuege Gesamtscore an
    s += "= " + getTotalScore();

    return s;
  }

}
