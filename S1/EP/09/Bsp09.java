// David Pape 01634454

class Bsp09 {
  public static void main(String[] args) {
    Cell c = new Cell(9, 0);
    // for (int i = 0; i < c.options.size(); i++) {
    //   System.out.println(c.options.get(i) + ",");
    // }
    System.out.println(c.getOptionCount());
    System.out.println(c.erase(9));
    System.out.println(c.getOptionCount());
  }
}
