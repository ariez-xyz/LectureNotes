//David Pape 01634454
public class FibonacciNumbers extends Sequence {

  public int getElement(int n) {
    if(n == 1)
      return 1;
    if(n == 0)
      return 0;
    return getElement(n - 2) + getElement (n - 1);
  }

  public String getMethodImplementedIn(){
    return "FibonacciNumbers";
  }
}
