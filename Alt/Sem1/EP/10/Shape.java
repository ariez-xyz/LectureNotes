//David Pape 01634454
public class Shape {
   double[][] m = {{1,0},{0,1}};
   double[] b = new double[2];

   public void shift(double s0, double s1) {
      b[0] = b[0] - m[0][0] * s0 - m[0][1] * s1;
      b[1] = b[1] - m[1][0] * s0 - m[1][1] * s1;
   }

   public void scale(double s0, double s1) {
      m[0][0] = m[0][0] / s0;
      m[0][1] = m[0][1] / s1;
      m[1][0] = m[1][0] / s0;
      m[1][1] = m[1][1] / s1;
   }

   public void rotate(double alpha) {
       double m00 = m[0][0];
       double m01 = m[0][1];
       double m10 = m[1][0];
       double m11 = m[1][1];

       m[0][0] =  m00 * Math.cos(alpha) + m01 * Math.sin(alpha);
       m[0][1] = -m00 * Math.sin(alpha) + m01 * Math.cos(alpha);
       m[1][0] =  m10 * Math.cos(alpha) + m11 * Math.sin(alpha);
       m[1][1] = -m10 * Math.sin(alpha) + m11 * Math.cos(alpha);
   }

   public boolean inside(double x0, double x1) {
      return basicInside((m[0][0] * x0 + m[0][1] * x1 + b[0]), (m[1][0] * x0 + m[1][1] * x1 + b[1]));
   }

   public boolean basicInside(double x0, double x1) {
      return false;
   }
}
