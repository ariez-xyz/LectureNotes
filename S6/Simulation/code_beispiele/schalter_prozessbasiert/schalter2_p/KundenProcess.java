package schalter2_p;

import desmoj.core.simulator.*;
import co.paralleluniverse.fibers.SuspendExecution;

// stellt die Kundenaktivitaeten als Prozess dar
public class KundenProcess extends SimProcess {

    // nuetzliche Referenz auf entsprechendes Modell
    private Schalter2_p_model meinModel;

    // Konstruktor
	  // Par 1: Modellzugehoerigkeit
	  // Par 2: Name des Ereignisses
	  // Par 3: show in trace?
    public KundenProcess(Model owner, String name, boolean showInTrace) {
        super(owner, name, showInTrace);

        meinModel = (Schalter2_p_model) owner;
    }

    
    // Beschreibung der Aktionen des Kunden vom Eintreffen bis zum Verlassen
    //   des Schalters 
    public void lifeCycle() throws SuspendExecution{

        // Kunde betritt Schalterraum -> in die Warteschlange geben
        meinModel.kundenReiheQueue.insert(this);
        sendTraceNote("Laenge der Kundenreihe: " + 
            meinModel.kundenReiheQueue.length());

        // Schalter frei? 
        if (!meinModel.freieSchalterQueue.isEmpty()) {
            // Schalter frei, von entsprechender WS holen
            SchalterProcess schalter = meinModel.freieSchalterQueue.first();
            // extra Entfernen von WS notwendig
            meinModel.freieSchalterQueue.remove(schalter);
            
            // Schalter sofort als naechsten Prozess aktivieren
            schalter.activateAfter(this);
            
            // Bedienvorgang ueber sich ergehen lassen
            passivate();
        }
        // Schalter besetzt
        else {
            // Kunde wartet in der WS
            passivate();
        }
        
        // Kunde wurde bedient und verlaesst den Schalterraum
        //  -> in diesem Beispiel nur eine Meldung sinnvoll
        sendTraceNote("Kunde wurde bedient und verlaesst den Schalterraum");
    }
}
