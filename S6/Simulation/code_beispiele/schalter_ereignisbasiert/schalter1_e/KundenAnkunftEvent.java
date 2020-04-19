package schalter1_e;

import desmoj.core.simulator.*;

// stellt das Ereignis einer Kundenankunft dar
public class KundenAnkunftEvent extends Event<KundeEntity> {

    // nuetzliche Referenz auf entsprechendes Modell
    private Schalter1_e_model meinModel;

    // Konstruktor
		// Par 1: Modellzugehoerigkeit
		// Par 2: Name des Ereignisses
		// Par 3: show in trace?
    public KundenAnkunftEvent(Model owner, String name, boolean showInTrace) {
        super(owner, name, showInTrace);

        meinModel = (Schalter1_e_model) owner;

    }
    
    // Beschreibung der Aktionen, die den Kunden nach dem Eintreffen beim
    // Schalter betreffen
    public void eventRoutine(KundeEntity kunde) {

        // cast notwendig
        //KundeEntity kunde =  who;

        // Kunde in Warteschlange
        meinModel.kundenReiheQueue.insert(kunde);
        sendTraceNote("Laenge der Kundenreihe: " + 
            meinModel.kundenReiheQueue.length());

        // Schalter frei?
        
        if (!meinModel.freieSchalterQueue.isEmpty()) {
            // Schalter frei, von entsprechender WS holen
            SchalterEntity schalter = meinModel.freieSchalterQueue.first();
            // extra Entfernen von WS notwendig
            meinModel.freieSchalterQueue.remove(schalter);
            
            // Schalter in entsprechende WS um Referenz nicht zu verlieren
            meinModel.besetzteSchalterQueue.insert(schalter);
            
            // Kunden aus Kundenreihe um am Schalter bedient zu werden
            // -> Referenz auf Kunden bereits vorhanden - kein first() n�tig!
            meinModel.kundenReiheQueue.remove(kunde);
            
            // Bedienungsende Ereignis erzeugen
            BedienEndeEvent bedienEnde = 
                new BedienEndeEvent (meinModel, "Bedienung Ende", true);
            // eintragen in Ereignisliste
            bedienEnde.schedule(kunde, new TimeSpan(meinModel.getBedienZeit()));
        }
    }
}
