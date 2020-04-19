package schalter1_e;

import desmoj.core.simulator.*;
import desmoj.core.dist.*;

/*
 main-Klasse vom einfachen Schalter-Modell (abgeleitet von 
 desmoj.core.simulator.Model) - stellt die notwendige Infrastruktur zur Verfuegung
*/

public class Schalter1_e_model extends Model {
	
	/**
	* Zufallszahlengenerator fuer Kundenankuenfte
	*/
	private ContDistExponential kundenAnkunftsZeit;

    /**
    * liefert eine Zufallszahl fuer Kundenankunftszeit
     */
    public double getKundenAnkunftsZeit() {
	   return kundenAnkunftsZeit.sample();
    }

	
    // Zufallszahlengenerator zur Ermittlung der Bedienzeit am Schalter
	private ContDistUniform bedienZeit;

    /**
    * liefert eine Zufallszahl fuer Bedienzeit
    */
    public double getBedienZeit() {
	   return bedienZeit.sample();
    }
    
	/**
    * Warteschlange fuer wartende Kunden
    * jeder Kunde kommt zuerst hier hinein
    *
    * liefert elementare Statistik
    */
    protected Queue<KundeEntity> kundenReiheQueue;


	/*
    * Warteschlange fuer freie Schalter
    * -> elementare Statistik erhueltlich
    * -> mehrere Schalter koennen verwaltet werden
	*/
	protected Queue<SchalterEntity> freieSchalterQueue;
	
	/**
    * Warteschlagen fuer besetzte Schalter
    * -> Referenzen auf besetzte Schalter gehen sonst verloren
    *    (beliebige andere Datenstruktur verwendbar, 
    *    jedoch Nutzen wie z.B. elementare Statistik)
    * -> zusaetzliche elementare Statistik moeglich
	*/
	protected Queue<SchalterEntity> besetzteSchalterQueue;

    /**
     * Konstruktor
    */
    public Schalter1_e_model(Model owner, String name, boolean showInReport, 
                            boolean showIntrace) {
    	super(owner, name, showInReport, showIntrace);
    }


    /**
     * Kurzbeschreibung des Modells
     */
    public String description() {
    	return "Schalter1_e Modell (Ereignis orientiert):" +
               "simuliert einen Bankschalter, wo ankommende Kunden zuerst in einer "+
               "Warteschlange eingereiht werden. Wenn der Schalter frei ist, "+
               "werden sie bedient.";
    }	


    /**
    * ersten Kunden erzeugen und in Ereignisliste eintragen
    * -> erste Kundenankunft
     */
    public void doInitialSchedules() {

        // erstes Kundenkreation-Eereignis erzeugen 
        NeuerKundeEvent ersterKunde =
			new NeuerKundeEvent(this, "Kundenkreation", true);

         // erstes Kundenkreations-Ereignis in Ereignisliste eintragen
         ersterKunde.schedule(new TimeSpan(getKundenAnkunftsZeit()));
    }


    /**
    * Initialisierung der benutzten DESMO-J Infrastruktur
     */
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
    	
    	//kundenAnkunftsZeit.setSeed(1234567890);

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
       	kundenReiheQueue = new Queue<KundeEntity>(this, "Kunden-Warteschlange",true, true);	
	
    	// Warteschlange fuer freie Schalter initialisieren
    	// Par 1: Modellzugehoerigkeit
    	// Par 2: Name der Warteschlange
    	// Par 3: show in report?
    	// Par 4: show in trace?
    	freieSchalterQueue = new Queue<SchalterEntity>(this, "freie Schalter WS",true, true);
	
        // den Schalter in freieSchalterQueue einfuegen
        // Hinweis: dies geschieht nicht in doInitialSchedules(), da keine
        //   Ereignisse erzeugt werden
    	SchalterEntity schalter;

        
        // Schalter erzeugen
    	// Par 1: Modellzugehoerigkeit
    	// Par 2: Name der Entitaet
    	// Par 3: show in trace?
    	for (int i = 1; i<=2; i++){
    	schalter = new SchalterEntity(this, "Bankschalter", true);
        // Schalter einfuegen
        freieSchalterQueue.insert(schalter);
    	} 
        
    	// Warteschlange fuer besetzte Schalter initialisieren
    	// Par 1: Modellzugehoerigkeit
    	// Par 2: Name der Warteschlange
    	// Par 3: show in report?
    	// Par 4: show in trace?
        besetzteSchalterQueue = new Queue<SchalterEntity>(this, "besetzte Schalter WS", true, true);
    }

    // Hauptmethode, zustaendig fuer
    // - Experiment instanziieren
    // - Modell instanziieren
    // - Modell mit Experiment verbinden
    //   - Einstellungen fuer Simulation und Ergebnisberichte
    //   - Simulation starten
    //   - Kriterium fuer Simulationsende aufstellen
    //   - Reports initialisieren
    //   - aufraeumen, abschliessen   
    public static void main(java.lang.String[] args) {
      int duration = 240000;

    	// neues Experiment erzeugen
    	// ATTENTION!
    	// Use as experiment name a OS filename compatible string!!
    	// Otherwise your simulation will crash!!
    	Experiment schalterExperiment = 
            new Experiment("Schalter1-ereignis");
 
        // neues Modell erzeugen
        // Par 1: null markiert main model, sonst Mastermodell angeben
        Schalter1_e_model sch_e_model = 
            new Schalter1_e_model(null, "Schalter Modell", true, true);  

        // Modell mit Experiment verbinden
        sch_e_model.connectToExperiment(schalterExperiment);

        // Intervall fuer trace/debug
        schalterExperiment.tracePeriod(new TimeInstant(0.0), new TimeInstant(60));
        schalterExperiment.debugPeriod(new TimeInstant(0.0), new TimeInstant(60));

        // Ende der Simulation setzen
        // -> hier 4 Stunden (= 240 min)
        schalterExperiment.stop(new TimeInstant(duration));

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
