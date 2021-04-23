/* -------------------------------------------------------------------------
 * A Monte Carlo simulation of Buffon's needle experiment.
 *
 * Name              : Buffon.java
 * Authors           : Steve Park & Dave Geyer
 * Translated by     : Jun Wang & Richard Dutton
 * Language          : Java
 * Latest Revision   : 6-16-06
 * ------------------------------------------------------------------------- 
 */

import java.io.*;
import java.util.*;
import java.lang.*;
import java.text.*;

class Buffon {

  static long   N       = 10000;              /* number of replications */
  static double HALF_PI = Math.PI / 2;        /* 1.5707963...           */
  static double R       = 1.0;                /* length of the needle   */


  public static void main(String[] args) {
      
    long   i;                                 /* replication index      */
    long   crosses = 0;                       /* number of crosses      */
    double p;                                 /* estimated probability  */
    double u, v;                              /* endpoints              */
    double theta;                             /* angle                  */
    long   seed;                              /* the initial rng seed   */
    long   j;

    Buffon b = new Buffon();
    Rng r = new Rng();

    r.putSeed(-1);                 /* any negative integer will do      */
    seed = r.getSeed();            /* trap the value of the intial seed */

    for (i = 0; i < N; i++) {
      u     = r.random();
      theta = b.uniform(-HALF_PI, HALF_PI, r);
      v     = u + R * Math.cos(theta);
      if (v > 1.0)
        crosses++;
    }

    p = (double) crosses / N;                 /* estimate the probability */

    DecimalFormat f = new DecimalFormat("###0.00");
    DecimalFormat g = new DecimalFormat("###0.000");

    System.out.println("\nbased on " + N + " replications and a needle of length " + f.format(R));
    System.out.println("with an initial seed of " + seed);
    System.out.println("the estimated probability of a cross is " + g.format(p) + "\n");
  }

  
   double uniform(double a, double b, Rng r) {
/* ------------------------------------------------
 * generate a Uniform random variate, use a < b 
 * ------------------------------------------------
 */
    return (a + (b - a) * r.random());
  }
}
