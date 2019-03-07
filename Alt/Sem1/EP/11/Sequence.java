//David Pape 01634454
public class Sequence {

  public int getElement(int n) {
    return -1;
  }

  public final int getPartialSum(int n) {
    int result = 0;

    for(int i = 0; i <= n; i++) {
      result += getElement(i);
      System.out.println("Element(" + i + ") = " + result + " ... Class = [" + getClass().getName() + "], Method implemented in [" + getMethodImplementedIn() + "]");
    }

    return result;
  }

  public String getMethodImplementedIn(){
    return "Sequence";
  }
}
