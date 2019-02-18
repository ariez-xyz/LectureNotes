package pape_sismanovic.model;

import pape_sismanovic.enums.EVENT_TYPE;

/**
 * Data object holding information about an atomic change
 * Passed by a subject to observers via notify()
 */
public class Event {
    private EVENT_TYPE type;
    private String affectedIsbn;
    private String newIsbn;

    public Event(EVENT_TYPE type, String affectedIsbn) {
        this.type = type;
        this.affectedIsbn = affectedIsbn;
    }

    public Event(EVENT_TYPE type, String affectedIsbn, String newIsbn) {
        this.type = type;
        this.affectedIsbn = affectedIsbn;
        this.newIsbn = newIsbn;
    }

    public EVENT_TYPE type() {
        return type;
    }

    public String affectedIsbn() {
        return affectedIsbn;
    }

    public String  newIsbn() {
        return newIsbn;
    }
}
