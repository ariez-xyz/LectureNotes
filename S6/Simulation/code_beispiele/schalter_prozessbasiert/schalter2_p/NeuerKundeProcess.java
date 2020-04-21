package schalter2_p;

import desmoj.core.simulator.*;
import co.paralleluniverse.fibers.SuspendExecution;

// stellt die Erscheinung eines neuen Kunden im System dar - als Prozess realisiert
// -> Beschreibung der Aktionen für einen neue Kundenankunft
public class NeuerKundeProcess extends SimProcess {

    // nuetzliche Referenz auf entsprechendes Modell
    private Schalter2_p_model meinModel;

    // Konstruktor
	  // Par 1: Modellzugehoerigkeit
	  // Par 2: Name des Ereignisses
	  // Par 3: show in trace?

    public NeuerKundeProcess (Model owner, String name, boolean showInTrace) {
	   super(owner, name, showInTrace);

	   meinModel = (Schalter2_p_model) owner;
    }
    
    // Aktionen, die bei Aktivierung dieses Ereignisses ausgefuehrt werden
    public void lifeCycle() throws SuspendExecution {
	
        while (true) {
            // Prozess deaktivieren bis naechster Kunde erzeugt werden soll
            hold (new TimeSpan(meinModel.getKundenAnkunftsZeit()));
     
            // neuen Kunden erzeugen
            KundenProcess neuerKunde = new KundenProcess (meinModel, "Kunde", true);
    
            // neuer Kunde betritt Schalterbereich -> Kunde ist zu aktivieren
            //  soll unmittelbar nach diesem Generator-Ereignis geschehen
            neuerKunde.activateAfter(this);
    
        }
    }
}
