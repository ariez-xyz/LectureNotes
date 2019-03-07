//David Pape 01634454
public class NoteList {
  public Note[] notes;

  //constructor with initial capacity
  public NoteList (int capacity) {
    if(capacity < 1)
      capacity = 1;

    notes = new Note[capacity];
  }

  //add Note n to the list
  public void add (Note n) {
    for(int i = 0; i < notes.length; i++)
      if(notes[i] == null) {
        notes[i] = n;
        return;
      }

    Note[] duplicate = new Note[notes.length * 2];
    for(int i = 0; i < notes.length; i++)
      duplicate[i] = notes[i];

    duplicate[notes.length] = n;
    notes = duplicate;
  }

  //returns the entry on the given position; returns null if there is none or idx is not a valid index
  public Note get (int idx) {
    try {
      return notes[idx];
    } catch(IndexOutOfBoundsException e) {
      return null;
    }
  }

  //returns the smallest index of Note n if n is in the list
  // - otherwise returns -1
  public int contains (Note n) {
    return getIndex(n, 0) - 1;
  }

  //returns the whole list as a string
  public String toString () {
    return "[" + stringify(0) + "]";
  }

  /**** methods for part 2 ****/

  //deletes Note n and returns true if it is in the list and keeps the list compact
  //returns false if Note n is not in list
  public boolean delete (Note n) {
    int index = getIndex(n, 0);

    if(index == -1)
      return false;

    moveDown(index);
    return true;
  }

  //returns some statistcs about the list
  public String getStatistics() {
    int entries = 0;
    for(Note n : notes)
      if(n != null)
        entries++;

    int duplicates = 0;

    //acts as a set of notes that we haven't considered yet
    NoteList clone = new NoteList(notes.length);
    for(Note n : notes)
      clone.add(n);

    for(int i = 0; i < notes.length; i++) {
      if(notes[i] == null)
        break;

      System.out.println("rn on: " + notes[i]);
      System.out.println("clone: " + clone);
      //since our garbage logic will count itself as a duplicate of itself
      //duplicates--;
      int o = duplicates;
      for(Note m : clone.notes)
        if(notes[i] != null && m != null && notes[i].equals(m) && -1 != getIndex(notes[i], contains(notes[i]) + 1)){
          duplicates++;
          System.out.println("duplicate: " + clone.getIndex(m, 0));
        }

      System.out.println("raised by: " + (duplicates - o));
      System.out.println("---");
      while(clone.delete(notes[i]));
    }

    String s = "List Statistics";
    s += "\n capacity: " + notes.length;
    s += "\n # of entries: " + entries;
    s += "\n # of doubling: " + duplicates;

    return s;
  }

  public int getIndex(Note n, int index) {
    if(notes[index] != null && notes[index].equals(n))
      return index;

    if(index + 1 < notes.length)
      return getIndex(n, index + 1);

    return -1;
  }

  public String stringify(int index) {
    if(index < notes.length && notes[index] != null)
      if(index == 0)
        return notes[index].toString() + stringify(index + 1);
      else
        return ", " + notes[index].toString() + stringify(index + 1);

    else
      return "";
  }

  public void moveDown(int index) {
    if(index < notes.length - 1 && notes[index] != null) {
      if(notes[index + 1] == null)
        notes[index] = null;
      else
        notes[index] = notes[index + 1];

      moveDown(index + 1);
    }
  }
}
