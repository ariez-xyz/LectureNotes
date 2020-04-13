
/* -------------------------------------------------------------------------
 * This program - an extension of program sis1.c - simulates a simple (s,S)
 * inventory system using Equilikely distributed demands.
 *
 * Name              : Sis2.java  (Simple Inventory System, version 2)
 * Authors           : Steve Park & Dave Geyer
 * Translated by     : Jun Wang & Richard Dutton
 * Language          : Java
 * Latest Revision   : 6-16-06
 *
 * Program sis2      : Section 3.1, based on Example 3.1.5
 * -------------------------------------------------------------------------
 */

import java.io.*;
import java.lang.*;
import java.text.*;


class Sis2Sum {                                /* sum of ...           */
  double setup;                                /*   setup instances       */
  double holding;                              /*   inventory held (+)    */
  double shortage;                             /*   inventory short (-)   */
  double order;                                /*   orders                */
  double demand;                               /*   demands               */

  void initSumParas() {
    setup     = 0.0;
    holding   = 0.0;
    shortage  = 0.0;
    order     = 0.0;
    demand    = 0.0;
  }
}

class Sis2 {
  static int MINIMUM     = 20;               /* 's' inventory policy parameter */
  static int MAXIMUM     = 80;               /* 'S' inventory policy parameter */
  static int STOP        = 100;              /* number of time intervals, 100  */
//  static int STOP = 10000;        /* number of time intervals, 100  */

  public static void main(String[] args) throws IOException {
      
    long index     = 0;                      /* time interval index     */
    long inventory = MAXIMUM;                /* current inventory level */
    long demand;                             /* amount of demand        */
    long order;                              /* amount of order         */
    Sis2Sum sum = new Sis2Sum();
    sum.initSumParas();

    Sis2 s = new Sis2();
    Rng r = new Rng();
    r.putSeed(123456789);
    
    while (index < STOP) {
      index++;

                                             
      /* add for example 4.2.5 ~ 4.2.7, and modify STOP = 10000 */
//      System.out.println(inventory);
                                             
      if (inventory < MINIMUM) {             /* place an order */
        order         = MAXIMUM - inventory;
        sum.setup++;
        sum.order    += order;
      }
      else                                   /* no order       */
        order         = 0;
      inventory      += order;               /* there is no delivery lag */
      demand          = s.getDemand(r);
      sum.demand     += demand;
      if (inventory > demand)
        sum.holding  += (inventory - 0.5 * demand);
      else {
        sum.holding  += s.sqr(inventory) / (2.0 * demand);
        sum.shortage += s.sqr(demand - inventory) / (2.0 * demand);
      }
      inventory     -= demand;
    }

    if (inventory < MAXIMUM) {               /* force the final inventory to */
      order      = MAXIMUM - inventory;      /* match the initial inventory  */
      sum.setup++;
      sum.order += order;
      inventory += order;
    }

    DecimalFormat f = new DecimalFormat("###0.00");

    System.out.print("\nfor " + index + " time intervals");
    System.out.println(" with an average demand of " + f.format(sum.demand / index));
    System.out.println("and policy parameters (s, S) = (" + MINIMUM + ", " + MAXIMUM + ")\n");
    System.out.println("   average order ............ =  " + f.format(sum.order / index));
    System.out.println("   setup frequency .......... =  " + f.format(sum.setup / index));
    System.out.println("   average holding level .... =  " + f.format(sum.holding / index));
    System.out.println("   average shortage level ... =  " + f.format(sum.shortage / index));
  }

  
  double sqr(long a) {
    return a * a;
  }
  
  long equilikely(long a, long b, Rng r) {
/* ------------------------------------------------
 * generate an Equilikely random variate, use a < b
 * ------------------------------------------------
 */
    return (a + (long) ((b - a + 1) * r.random()));
  }


   long getDemand(Rng r) {
/* ------------------------
 * generate the next demand
 * ------------------------
 */
    return (equilikely(10, 50, r));

    /* Modify for example 4.2.6 */
//    return (equilikely(5, 25, r) + equilikely(5, 25, r));
  }
  
}
