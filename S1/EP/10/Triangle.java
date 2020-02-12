//David Pape 01634454
public class Triangle extends Shape {
    public boolean basicInside(double x0, double x1) {
        return x1 <= 1 - Math.abs(x0) && x1 >= 0;
    }
}
