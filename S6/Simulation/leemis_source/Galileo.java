/* -------------------------------------------------------------------------
 * A Monte Carlo simulation of Galileo's three dice experiment.
 *
 * Name              : Galileo.java
 * Authors           : Steve Park & Dave Geyer
 * Translated by     : Jun Wang & Richard Dutton
 * Language          : Java
 * Latest Revision   : 6-16-06
 * ------------------------------------------------------------------------- 
 */

import java.io.*;
import java.util.*;
import java.text.*;


class Galileo {

  static long N = 10000;                    /* number of replications */

  public static void main(String[] args) {
      
    long i;                                 /* replication index      */
    int  x;                                 /* sum of three dice      */
    long count[] = new long [19];           /* histogram              */
    double p[] = new double [19];           /* probability estimates  */

    Galileo g = new Galileo();
    Rng r = new Rng();

    r.putSeed(0);

    for (i = 0; i < N; i++) {
      x = g.equilikely(1, 6, r) + g.equilikely(1, 6, r) + g.equilikely(1, 6, r);
      count[x]++;
    }

    for (x = 3; x < p.length; x++)          /* estimate probabilities */
      p[x] = (double) count[x] / N;

    DecimalFormat f = new DecimalFormat("###0.000");

    System.out.print("\nbased on " + N + " replications ");
    System.out.println("the estimated probabilities are:\n");
    for (x = 3; x < p.length; x++)
      System.out.println("p[" + x + "] = " + f.format(p[x]));
    System.out.println("");
  }
  
  int equilikely(long a, long b, Rng r) {
/* ------------------------------------------------
 * generate an Equilikely random variate, use a < b
 * ------------------------------------------------
 */
    return (int) (a + (long) ((b - a + 1) * r.random()));
  }
    
}
