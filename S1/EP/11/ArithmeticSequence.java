//David Pape 01634454
public class ArithmeticSequence extends Sequence {
  int firstElement;
  int difference;

  public ArithmeticSequence(int a, int d) {
    firstElement = a;
    difference = d;
  }

  public int getElement(int n) {
    if(n > 0)
      return getElement(n - 1) + difference;
    else
      return firstElement;
  }

  public String getMethodImplementedIn(){
    return "ArithmeticSequence";
  }
}
