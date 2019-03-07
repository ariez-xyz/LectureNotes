//David Pape 01634454
public class Star extends Shape{
    Triangle t;
    int points;
    double radius;

    public Star(int points, double smallRadius) {
        this.points = points;
        this.radius = smallRadius;

        t = new Triangle();

        double yOffset = radius * Math.cos(Math.PI / points);

        //REIHENFOLGE OMFG
        t.scale(radius * Math.sin(Math.PI / points), 1 - yOffset);
        t.shift(0, yOffset);
    }

    public boolean basicInside(double x0, double x1) {
        for (int i = 0; i < points; i++) {
            if (t.inside(x0, x1))
                return true;

            t.rotate(2 * Math.PI / points);
        }

        return Math.sqrt(x0*x0 + x1*x1) <= radius;
    }
}
