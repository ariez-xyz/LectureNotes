// David Pape 01634454

public class Bsp03 {

  public static void main(String[] args) {
    double summe = checkVar(SavitchIn.readLineDouble(), 10000, 30000);
    int laufzeit = (int) checkVar(SavitchIn.readLineDouble(), 10, 15);
    double zins = checkVar(SavitchIn.readLineDouble(), 2, 10) * 0.01;

    double rate = getCreditRate(summe, laufzeit, zins);
    System.out.println("Jaehrliche Rate: " + Math.round(rate));

    double rest = summe;
    double totale = 0;

    for (int i = 1; i <= laufzeit; i++) {
      double zinssumme = rest * zins;

      rest += zinssumme - rate;
      totale += rate;

      System.out.println("Jahr " + i + " - Restbetrag: " + Math.round(rest));
    }

    System.out.println("Statistik: ");
    System.out.println("Kreditsumme: " + (int) summe);
    System.out.println("Rueckzahlungen: " + Math.round(totale));
    System.out.println("Davon Zinsen: " + Math.round(totale - summe));

    double input = SavitchIn.readLineDouble();
    int erhoehung = (int) checkVar(input, 4, laufzeit - 2);
    rest = summe;
    totale = 0;

    for (int i = 1; i <= laufzeit; i++) {
      double zinssumme = rest * zins;

      rest += zinssumme - rate;
      totale += rate;

      System.out.println("Jahr " + i + " - Restbetrag: " + Math.round(rest));

      if (i == erhoehung) {
        zins += 0.02;
        rate = getCreditRate(rest, laufzeit - i, zins);
        System.out.println("Neue Rate: " + Math.round(rate));
      }
    }

    System.out.println("Statistik: ");
    System.out.println("Kreditsumme: " + (int) summe);
    System.out.println("Rueckzahlungen: " + Math.round(totale));
    System.out.println("Davon Zinsen: " + Math.round(totale - summe));
  }

  // calculate rate of fixed-rate credit
  public static double getCreditRate(double amountBorrowed, int mortageTime, double interestRate) {
    return (amountBorrowed * Math.pow((1 + interestRate), mortageTime) * interestRate) / (Math.pow((1 + interestRate), mortageTime) - 1);
  }

  public static double checkVar(double i, double lower, double upper) {
    if (i < lower) return lower;
    else if (i > upper) return upper;
    return i;
  }
}
