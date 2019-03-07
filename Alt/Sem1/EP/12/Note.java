//David Pape 01634454
public class Note {
  String note;

  public Note(String data) {
    note = data;
  }

  public String toString() {
    return "(" + note + ")";
  }

  public boolean equals(Note n) {
    return this.toString().equals(n.toString());
  }
}
