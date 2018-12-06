package pape_sismanovic.interfaces;

import pape_sismanovic.model.Event;

public interface Observer {
    /**
     * Receive a notification from a subject this observer is subscribed to
     * @param e The event the notification is about
     */
    void update(Event e);
}
