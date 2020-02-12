//David Pape 01634454

public class Bsp02 {

  static int gebuehr, kosten, teiln;

  public static void main(String[] args) {

    gebuehr = SavitchIn.readLineInt();
    kosten = SavitchIn.readLineInt();
    teiln = SavitchIn.readLineInt();
    System.out.print("Besser: ");

    if(findeGewinn(teiln + 1) < findeGewinn(teiln)){
      System.out.println("Teilnehmer: " + teiln + ", Gewinn: " + findeGewinn(teiln));
      System.out.println("Optimum: Teilnehmer: " + teiln + ", Gewinn: " + findeGewinn(teiln));
    }
    else{
      System.out.println("Teilnehmer: " + (teiln + 1) + ", Gewinn: " + findeGewinn(teiln + 1));

      int optimum = teiln + 1;
      while(findeGewinn(optimum + 1) >= findeGewinn(optimum))
        optimum++;

      System.out.println("Optimum: Teilnehmer: " + optimum + ", Gewinn: " + findeGewinn(optimum));
    }
  }

  public static int findeKosten(int n){
    return ((n * (n - 1)) / 2)*kosten;
  }

  public static int findeUmsatz(int n){
    return n * gebuehr;
  }

  public static int findeGewinn(int n){
    return findeUmsatz(n) - findeKosten(n);
  }

}
