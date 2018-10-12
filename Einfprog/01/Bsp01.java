//David Pape 01634454
public class Bsp01 {

    public static void main (String[] args) {
        //Teil 1
        p("Koerpergewicht [kg]: ");
        double koerpergewicht = SavitchIn.readLineInt();
        p("Radgewicht [kg]: ");
        double radgewicht = SavitchIn.readLineInt();
        p("Hoehe [m]: ");
        double hoehe = SavitchIn.readLineInt();
        p("Zeit [min]: ");
        double zeit = SavitchIn.readLineInt();
        double zeitS = zeit * 60;

        double gesamtgewicht = koerpergewicht + radgewicht;
        double anziehungskraft = 9.81f * gesamtgewicht;
        double arbeit = anziehungskraft * hoehe;
        double leistung = arbeit / zeitS;

        pl("Leistung [W]: " + leistung);

        //Teil 2
        double neuesGewicht = ((zeit - 5) * 60 * leistung) / (9.81 * hoehe) - radgewicht;
        double neueZeit = zeit - 5;
        pl("Gewicht fuer " + neueZeit + " min: " + neuesGewicht);
    }

    public static void p (Object o) {
        System.out.print(o);
    }

    public static void pl (Object o) {
        System.out.println(o);
    }
}
