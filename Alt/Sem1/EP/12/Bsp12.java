class Bsp12 {
  public static void main(String[] args) {
    NoteWithMail n = new NoteWithMail("as", "df");
    NoteWithMail m = new NoteWithMail("as", "de");
    NoteList l = new NoteList(10);

    l.add(n);
    l.add(m);
    l.add(new NoteWithMail("huren", "sohn"));
    l.add(new NoteWithMail("huren", "sohn"));
    l.add(new NoteWithMail("huren", "sohn"));
    l.add(new NoteWithMail("huren", "sohn"));

    System.out.println(l.getStatistics());
    System.out.println(l);
  }
}
