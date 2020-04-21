package schalter2_p;

import desmoj.core.simulator.*;
import co.paralleluniverse.fibers.SuspendExecution;

// stellt die Schalteraktivitäten als Prozess dar
public class SchalterProcess extends SimProcess {

    // nuetzliche Referenz auf entsprechendes Modell
    private Schalter2_p_model meinModel;

    // Konstruktor
	  // Par 1: Modellzugehoerigkeit
	  // Par 2: Name des Ereignisses
	  // Par 3: show in trace?
    public SchalterProcess(Model owner, String name, boolean showInTrace) {
        super(owner, name, showInTrace);

        meinModel = (Schalter2_p_model) owner;
    }
    
    // Beschreibung der Schalteraktivitaeten 
    public void lifeCycle() throws SuspendExecution {
        
        // Schalter ist immer in Aktion -> Endlosschleife
        while (true){
            
            // kein Kunde wartet
            if (meinModel.kundenReiheQueue.isEmpty()) {
                
                // Schalter in entsprechende WS
                meinModel.freieSchalterQueue.insert(this);
                
                // abwarten weiterer Aktionen
                passivate();
            }
            
            // Kunde wartet
            else {
                
                // ersten Kunden aus WS entfernen
                KundenProcess kunde = meinModel.kundenReiheQueue.first();
                meinModel.kundenReiheQueue.remove(kunde);
                
                // Kunde wird bedient
                // -> Prozess wird solange inaktiv gestellt
                hold(new TimeSpan(meinModel.getBedienZeit()));
                
                // Kunde wurde bedient, kann den Schalter verlassen
                // -> muss reaktiviert werden (für Abschlussaktionen)
                kunde.activate(new TimeSpan(0.0));
            }
        }
    }
}
