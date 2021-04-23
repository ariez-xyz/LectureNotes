/* -------------------------------------------------------------------------
 * This program is a next-event simulation of a simple inventory system with
 * a Poisson demand process, backlogging, and a Uniform(0,1) delivery lag.
 *
 * Name              : Sis3.java  (Simple Inventory System, version 3)
 * Authors           : Steve Park & Dave Geyer 
 * Translated by     : Jun Wang
 * Language          : Java
 * Latest Revision   : 6-16-06
 * ------------------------------------------------------------------------- 
 */

import java.io.*;
import java.lang.*;
import java.text.*;


class Sis3Sum {                               /* sum of ...              */
  double setup;                               /*   setup instances       */
  double holding;                             /*   inventory held (+)    */
  double shortage;                            /*   inventory short (-)   */
  double order;                               /*   orders                */
  double demand;                              /*   demands               */
  double lag;                                 /*   lags                  */

  void initSumParas() {
    setup     = 0.0;
    holding   = 0.0;
    shortage  = 0.0;
    order     = 0.0;
    demand    = 0.0;
    lag       = 0.0;
  }
}

class Sis3T{
  double demand;                      /* next demand time                */
  double review;                      /* next inventory review time      */
  double arrive;                      /* next order arrival time         */
  double current;                     /* current time                    */
  double next;                        /* next (most imminent) event time */
}

class Sis3 {
  static int MINIMUM     = 20;                 /* 's' inventory policy parameter >= 0 */
  static int MAXIMUM     = 80;                 /* 'S' inventory policy parameter > s  */
  static double START    = 0.0;
  static double STOP     = 100.0;
  static double INFINITY = 100.0 * STOP;

  static double time = START;
  

  public static void main(String[] args) {
      
    long inventory = MAXIMUM;            /* current inventory level */
    long order     = 0;                  /* current order level     */
    
    Sis3 s = new Sis3();
    Rngs r = new Rngs();
    r.plantSeeds(0);

    Sis3Sum sum = new Sis3Sum();
    sum.initSumParas();
    
    Sis3T t = new Sis3T();
    t.current = START;
    t.demand  = s.getDemand(r);           /* schedule the first demand */
    t.review  = t.current + 1.0;          /* schedule the first review */
    t.arrive  = INFINITY;                 /* no order arrival pending  */


    while (t.current < STOP) {
      t.next          = Math.min(t.demand, t.review);
      t.next          = Math.min(t.next, t.arrive);
      if (inventory > 0)
        sum.holding  += (t.next - t.current) * inventory;
      else
        sum.shortage -= (t.next - t.current) * inventory;
      t.current       = t.next;

      if (t.current == t.demand) {        /* process an inventory demand */
        sum.demand++;
        inventory--;
        t.demand = s.getDemand(r);
      }

      else if (t.current == t.review) {   /* process inventory review */
        if (inventory < MINIMUM) {
          double lag;
          order      = MAXIMUM - inventory;
          lag        = s.getLag(r);
          sum.setup++;
          sum.order += order;
          sum.lag   += lag;
          t.arrive   = t.current + lag;
        }
        t.review  = t.current + 1.0;
      }

      else {                           /* process an inventory order arrival*/
        inventory += order;
        order      = 0;
        t.arrive   = INFINITY;
      }
    }

    if (inventory < MAXIMUM) {          /* adjust the final inventory level */
      order      = MAXIMUM - inventory;
      sum.setup++;
      sum.order += order;
      inventory += order;
    }

    DecimalFormat f = new DecimalFormat("###0.00");

    System.out.print("\nfor " + (long) STOP + " time intervals");
    System.out.println(" with an average demand of " + f.format(sum.demand / STOP));
    if (sum.setup > 0.0)
      System.out.print("an average lag of " + f.format(sum.lag / sum.setup));
    System.out.println(" and policy parameters (s, S) = (" + MINIMUM + ", " + MAXIMUM + ")\n");
    System.out.println("   average order ............ =  " + f.format(sum.order / STOP));
    System.out.println("   setup frequency .......... =  " + f.format(sum.setup / STOP));
    System.out.println("   average holding level .... =  " + f.format(sum.holding / STOP));
    System.out.println("   average shortage level ... =  " + f.format(sum.shortage / STOP) + "\n");
  }

  
  double exponential(double m, Rngs r) {
/* ---------------------------------------------------
 * generate an Exponential random variate, use m > 0.0
 * ---------------------------------------------------
 */
    return (-m * Math.log(1.0 - r.random()));
  }

  double uniform(double a, double b, Rngs r) {
/* --------------------------------------------
 * generate a Uniform random variate, use a < b
 * --------------------------------------------
 */
    return (a + (b - a) * r.random());
  }


  double getDemand(Rngs r) {
/* --------------------------------------------------------------
 * generate the next demand instance (time) with rate 30 per time
 * interval and exactly one unit of demand per demand instance
 * --------------------------------------------------------------
 */
    r.selectStream(0);
    time += exponential(1.0 / 30.0, r);
    return (time);
  }


  double getLag(Rngs r) {
/* ------------------------------
 * generate a delivery lag (time)
 * ------------------------------
 */
    r.selectStream(1);
    return (uniform(0.0, 1.0, r));
  }  
}
