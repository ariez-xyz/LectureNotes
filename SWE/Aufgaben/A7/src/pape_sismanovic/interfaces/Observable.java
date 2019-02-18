package pape_sismanovic.interfaces;

import pape_sismanovic.model.Event;

public interface Observable {
    /**
     * @param o Observer to be added to notification list
     * @return True on success, False if o is already on list
     */
    boolean registerObserver (Observer o);

    /**
     * @param o Observer to be deleted from notification list
     * @return True on success, False if o is not on list
     */
    boolean removeObserver (Observer o);

    /**
     * Notify observers of some event
     * @param e Event to be sent out to observers
     */
    void notifyObservers (Event e);
}
