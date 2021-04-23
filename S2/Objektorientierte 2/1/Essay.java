//Filip Vecek     11700962
//Johannes Spilka 11724817
//David Pape      01634454

public class Essay implements Measurable {
  String text;

  public Essay(String text) {
    this.text = text;
  }

  //zaehle die Anzahl Woerter
  public double getMeasure() {
    int wordCount = 0;
    char[] text = this.text.toCharArray();
    int j = 0;

    //iteriere ueber den gesamten Text
    for(int i = 0; i < text.length; i++) {
      //gehe vorwaerts, bis ein Nicht-Buchstabe auftaucht
      while(j < text.length && (Character.isLetter(text[j]) || text[j] == 39))
        j++;

      //gehe vorwaerts, bis ein Buchstabe auftaucht
      while(j < text.length && !Character.isLetter(text[j]))
        j++;

      wordCount++;

      //aktualisiere den Index der for-Schleife
      i = j;
    }

    return wordCount;
  }
}
