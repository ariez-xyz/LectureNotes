//David Pape 01634454
public class NoteWithMail extends Note {
  String mail;

  public NoteWithMail (String data, String mail) {
    super(data);
    this.mail = mail;
  }

  public String toString() {
    return "(" + note + "|" + mail + ")";
  }
}
