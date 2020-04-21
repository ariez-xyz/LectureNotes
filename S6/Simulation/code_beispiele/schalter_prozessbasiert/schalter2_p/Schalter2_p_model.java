package schalter2_p;

import desmoj.core.simulator.*;
import desmoj.core.dist.*;

/*
 main-Klasse vom einfachen Schalter-Modell (abgeleitet von 
 desmoj.core.simulator.Model) - stellt die notwendige Infrastruktur zur Verfuegung
*/


public class Schalter2_p_model extends Model {
	
	// Zufallszahlengenerator fuer Kundenankuenfte
	private ContDistExponential kundenAnkunftsZeit;

    // liefert eine Zufallszahl fuer Kundenankunftszeit
    public double getKundenAnkunftsZeit() {
	   return kundenAnkunftsZeit.sample();
    }
	
    // Zufallszahlengenerator zur Ermittlung der Bedienzeit am Schalter
	private ContDistUniform bedienZeit;

    // liefert eine Zufallszahl fuer Bedienzeit
    public double getBedienZeit() {
        return bedienZeit.sample();
    }
    
	/**
    * Warteschlange fuer wartende Kunden
    * jeder Kunde kommt zuerst hier hinein
    *
    * liefert elementare Statistik
    */
    protected ProcessQueue<KundenProcess> kundenReiheQueue;


	/*
    * Warteschlange fuer freie Schalter
    * -> elementare Statistik erhaeltlich
    * -> mehrere Schalter koennen verwaltet werden
	*/
	protected ProcessQueue<SchalterProcess> freieSchalterQueue;
	

     // Konstruktor
    public Schalter2_p_model(Model owner, String name, boolean showInReport, 
                            boolean showIntrace) {
    	super(owner, name, showInReport, showIntrace);
    }

     // Kurzbeschreibung des Modells
    public String description() {
    	return "Schalter2_p Model (Prozess orientiert)l:" +
               "simuliert einen Bankschalter, wo ankommende Kunden zuerst in einer"+
               "Warteschlange eingereiht werden. Wenn der Schalter frei ist,"+
               "werden sie bedient.";
    }	


    // erste Ereignisse eintragen f�r Simulationsbeginn
    public void doInitialSchedules() {

        // Prozess zur Erzeugung von Kunden einrichten
        NeuerKundeProcess neuerKunde = 
            new NeuerKundeProcess(this, "Kundenkreation", true);
        // Prozess starten
        neuerKunde.activate(new TimeSpan(0.0));
                 
        // Schalter einrichten
        SchalterProcess schalter = new SchalterProcess(this, "Schalter", true);
        // Schalterprozess starten (= "Schalter wird eroeffnet")
        schalter.activate(new TimeSpan(0.0));
    }


    // Initialisierung der benutzten DESMO-J Infrastruktur
    public void init() {
		
    	// Generator fuer Ankunftszeiten initialisieren
    	// Par 1: Modellzugehoerigkeit
    	// Par 2: Name des Generators
    	// Par 3: mittlere Zeitdauer in Minuten zwischen Kundenankuenften
    	// Par 4: show in report?
    	// Par 5: show in trace?
    	kundenAnkunftsZeit = 
            new ContDistExponential(this, "Ankunftszeitintervall", 3.0, true, true);	

    	// negative Ankunftszeitintervalle sind nicht moeglich, 
    	// jedoch liefert Exponentialverteilung auch negative Werte, daher
    	kundenAnkunftsZeit.setNonNegative(true);

    	// Generator fuer Bedienzeiten initialisieren
    	// Par 1: Modellzugehoerigkeit
    	// Par 2: Name des Generators
    	// Par 3: minimale Bedienzeit in Minuten
    	// Par 4: maximale Bedienzeit in Minuten
    	// Par 5: show in report?
    	// Par 6: show in trace?
        bedienZeit = 
            new ContDistUniform(this, "Bedienzeiten", 0.5, 10.0, true, true);	

    	// Warteschlange fuer Kunden initialisieren
    	// Par 1: Modellzugehoerigkeit
    	// Par 2: Name der Warteschlange
    	// Par 3: show in report?
    	// Par 4: show in trace?
       	kundenReiheQueue = new ProcessQueue<KundenProcess>(this, "Kunden-Warteschlange",true, true);	
	
    	// Warteschlange fuer freie Schalter initialisieren
    	// Par 1: Modellzugehoerigkeit
    	// Par 2: Name der Warteschlange
    	// Par 3: show in report?
    	// Par 4: show in trace?
    	freieSchalterQueue = new ProcessQueue<SchalterProcess>(this, "freie Schalter WS",true, true);
	
    }

    // Hauptmethode, zustaendig fuer
    // - Experiment instantieren
    // - Modell instantieren
    // - Modell mit Experiment verbinden
    //   - Einstellungen fuer Simulation und Ergebnisberichte
    //   - Simulation starten
    //   - Kriterium fuer Simulationsende aufstellen
    //   - Reports initialisieren
    //   - aufraeumen, abschliessen   
    public static void main(java.lang.String[] args) {

    	// neues Experiment erzeugen
    	// ATTENTION!
    	// Use as experiment name a OS filename compatible string!!
    	// Otherwise your simulation will crash!!
    	Experiment schalterExperiment = 
            new Experiment("Schalter2-Prozess");
 
        // neues Modell erzeugen
        // Par 1: null markiert main model, sonst Mastermodell angeben
        Schalter2_p_model sch_p_model = 
            new Schalter2_p_model(null, "Schalter Modell", true, true);  

        // Modell mit Experiment verbinden
        sch_p_model.connectToExperiment(schalterExperiment);

        // Intervall fuer trace/debug
        schalterExperiment.tracePeriod(new TimeInstant(0.0), new TimeInstant(60));
        schalterExperiment.debugPeriod(new TimeInstant(0.0), new TimeInstant(60) );

        // Ende der Simulation setzen
        // -> hier 4 Stunden (= 240 min)
        schalterExperiment.stop(new TimeInstant(240));

        // Experiment zur Zeit 0.0 starten
        schalterExperiment.start(); 

        // -> Simulation laeuft bis Abbruchkriterium erreicht ist
        // -> danach geht es hier weiter

        // Report generieren
        schalterExperiment.report();

        // Ausgabekanaele schliessen, allfaellige threads beenden
        schalterExperiment.finish();
	
    }
}
