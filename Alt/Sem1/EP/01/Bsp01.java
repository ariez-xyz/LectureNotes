public class Bsp01 {
  public static void main(String[] args){
    System.out.print("A: ");
    double a = SavitchIn.readLineDouble();
    System.out.print("B: ");
    double b = SavitchIn.readLineDouble();
    System.out.print("C: ");
    double c = SavitchIn.readLineDouble();

    double aPerc = a/(a+b+c);
    double bPerc = b/(a+b+c);
    double cPerc = c/(a+b+c);

    System.out.println("Prozent A: " + aPerc * 100);
    System.out.println("Prozent B: " + bPerc * 100);
    System.out.println("Prozent C: " + cPerc * 100);

    System.out.print("Sitze: ");
    double sitze = SavitchIn.readLineDouble();
    
    int mandateA = (int) (sitze*aPerc+0.00001);
    int mandateB = (int) (sitze*bPerc+0.00001);
    int mandateC = (int) (sitze*cPerc+0.00001);

    System.out.println("Mandate A: " + mandateA); 
    System.out.println("Mandate B: " + mandateB);
    System.out.println("Mandate C: " + mandateC);

    System.out.println("Restmandate: " + (int)(sitze - mandateA - mandateB - mandateC));
  }
}
