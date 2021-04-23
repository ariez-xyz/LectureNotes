/* -------------------------------------------------------------------------
 * A Monte Carlo simulation of the dice game Craps.
 *
 * Name              : Craps.java
 * Authors           : Steve Park & Dave Geyer 
 * Translated by     : Jun Wang & Richard Dutton
 * Language          : Java
 * Latest Revision   : 6-16-06
 * ------------------------------------------------------------------------- 
 */

import java.io.*;
import java.util.*;
import java.text.*;


class Craps {

  static long N = 10000;                    /* number of replications */
//  static int  C = 1000;          /* for example 4.2.8      */

  public static void main(String[] args) {
      
    long i;                                 /* replication index      */
    int  wins = 0;                          /* number of wins         */
    int  roll;                              /* sum of two dice        */
    int  point;
//    int  j = 0;                  /* for example 4.2.8      */

    Galileo g = new Galileo();
    Rng r = new Rng();

    r.putSeed(0);                   /* 555555555    987654321 */

//    for (j = 0; j < C; j++) {    /* for example 4.2.8      */
//      wins = 0;

    for (i = 1; i <= N; i++) {
      roll = g.equilikely(1, 6, r) + g.equilikely(1, 6, r);
      if (roll == 7 || roll == 11)
        wins ++;
      else if (roll != 2 && roll != 3 && roll != 12) {
        point = roll;
        do {
          roll = g.equilikely(1, 6, r) + g.equilikely(1, 6, r);
          if (roll == point)
            wins ++;
        } while (roll != point && roll != 7);
      }
    }

    DecimalFormat f = new DecimalFormat("###0.000");
  
    System.out.println("\nfor " + N + " replications");
    System.out.println("the estimated probability of winning is " + f.format((double) wins/N));
//    System.out.println((double) wins/N);
//    }                            /* for example 4.2.8      */
  }
  
  int equilikely(long a, long b, Rng r) {
/* ------------------------------------------------
 * generate an Equilikely random variate, use a < b
 * ------------------------------------------------
 */
    return (int) (a + (long) ((b - a + 1) * r.random()));
  }
    
}
