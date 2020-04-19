package schalter1_e;

import desmoj.core.simulator.*;

// stellt das Ende eine Bedienvorgangs am Schlater dar
public class BedienEndeEvent extends Event<KundeEntity> {

    // nuetzliche Referenz auf entsprechendes Modell
    private Schalter1_e_model meinModel;

    // Konstruktor
  	// Par 1: Modellzugehoerigkeit
	  // Par 2: Name des Ereignisses
	  // Par 3: show in trace?
    public BedienEndeEvent(Model owner, String name, boolean showInTrace) {
        super(owner, name, showInTrace);

        meinModel = (Schalter1_e_model) owner;
    }
    
    // Beschreibung der Aktionen, die den Kunden nach Beendigung der
    // Bedienung am Schalter betreffen
 
    public void eventRoutine(KundeEntity kunde) {
        
        // cast notwendig
        //KundeEntity kunde = (KundeEntity) who;
        
        // in diesem einfachen Fall keinerlei Aktionen fuer Kunde notwendig 
        // -> automatisch zum garbage collector

        // wartet ein weiterer Kunde auf Bedienung?
        if (!meinModel.kundenReiheQueue.isEmpty()) {
            // Kunde vorhanden, aus Kundenreihe entfernen
            KundeEntity naechsterKunde = 
            	meinModel.kundenReiheQueue.first();
            meinModel.kundenReiheQueue.remove(naechsterKunde);
            
            // Bedienungsende Ereignis erzeugen
            BedienEndeEvent bedienEnde = 
                new BedienEndeEvent (meinModel, "Bedienung Ende", true);
            // eintragen in Ereignisliste
            bedienEnde.schedule(kunde, new TimeSpan(meinModel.getBedienZeit()));
        }
        else {
            // kein Kunde wartet
            // Schalter frei -> aus entsprechender WS
            SchalterEntity schalter = meinModel.besetzteSchalterQueue.first();
            meinModel.besetzteSchalterQueue.remove(schalter);
            
            // ... in entsprechende WS
            meinModel.freieSchalterQueue.insert(schalter);
        }
    }
}
