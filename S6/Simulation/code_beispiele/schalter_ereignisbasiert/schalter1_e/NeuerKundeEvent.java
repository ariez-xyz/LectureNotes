package schalter1_e;

import desmoj.core.simulator.*;

// stellt die Erscheinung eines neuen Kunden im System dar
// -> Beschreibung der Aktionen fuer eine neue Kundenankunft
public class NeuerKundeEvent extends ExternalEvent {

    // nuetzliche Referenz auf entsprechendes Modell
    private Schalter1_e_model meinModel;

    // Konstruktor
	  // Par 1: Modellzugehoerigkeit
	  // Par 2: Name des Ereignisses
	  // Par 3: show in trace?

    public NeuerKundeEvent (Model owner, String name, boolean showInTrace) {
	   super(owner, name, showInTrace);

	   meinModel = (Schalter1_e_model) owner;
    }
    
    // Aktionen, die bei Aktivierung dieses Ereignisses ausgefuehrt werden
    public void eventRoutine() {
	
        // neuen Kunden erzeugen
        KundeEntity kunde = new KundeEntity (meinModel, "Kunde", true);

        // neues KundenAnkunfts-Ereignis erzeugen
        KundenAnkunftEvent kundenAnkunft =
            new KundenAnkunftEvent (meinModel, "Kundenankunft", true);

        // dieses aktivieren
        kundenAnkunft.schedule(kunde, new TimeSpan(0.0));

        // neues Ereignis fuer naechsten Kunden erzeugen eintragen
        NeuerKundeEvent neuerKunde = 
            new NeuerKundeEvent(meinModel, "Kundenkreation", true);
        neuerKunde.schedule (new TimeSpan(meinModel.getKundenAnkunftsZeit()));

    }
}
