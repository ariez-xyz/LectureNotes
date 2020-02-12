//David Pape 01634454
public class Ring extends Shape {
    double radius = 1;
    double innerRadius;

    public Ring(double smallRadius) {
        innerRadius = smallRadius;
    }

    public boolean basicInside(double x0, double x1) {
        double distance = Math.sqrt(x0*x0 + x1*x1);

        return innerRadius <= distance && distance <= radius;
    }
}
