/* -------------------------------------------------------------------------
 * This program - an extension of program ssq1.c - simulates a single-server
 * FIFO service node using Exponentially distributed interarrival times and
 * Uniformly distributed service times (i.e. a M/U/1 queue).
 *
 * Name              : Ssq2.java  (Single Server Queue, version 2)
 * Authors           : Steve Park & Dave Geyer
 * Translated by     : Jun Wang & Richard Dutton
 * Language          : Java
 * Latest Revision   : 6-16-06
 * ------------------------------------------------------------------------- 
 */

import java.io.*;
import java.lang.*;
import java.text.*;


class Ssq2Sum {                                  /* sum of ...           */
  double delay;                                  /*   delay times        */
  double wait;                                   /*   wait times         */
  double service;                                /*   service times      */
  double interarrival;                           /*   interarrival times */

  void initSumParas() {
    delay = 0.0;
    wait = 0.0;
    service = 0.0;
    interarrival = 0.0;
  }
}

class Ssq2 {

  static long LAST = 10000;                    /* number of jobs processed */
  static double START = 0.0;                   /* initial time             */
  
  static double sarrival = START;              /* Why did I do this?       */

  public static void main(String[] args) {
    
    long   index     = 0;                         /* job index            */
    double arrival   = START;                     /* time of arrival      */
    double delay;                                 /* delay in queue       */
    double service;                               /* service time         */
    double wait;                                  /* delay + service      */
    double departure = START;                     /* time of departure    */
    Ssq2Sum sum = new Ssq2Sum();
    sum.initSumParas();
      
    Ssq2 s = new Ssq2();
    Rng r = new Rng();
    r.putSeed(123456789);

    while (index < LAST) {
      index++;
      arrival      = s.getArrival(r);
      if (arrival < departure)
        delay      = departure - arrival;         /* delay in queue    */
      else
        delay      = 0.0;                         /* no delay          */
      service      = s.getService(r);
      wait         = delay + service;
      departure    = arrival + wait;              /* time of departure */
      sum.delay   += delay;
      sum.wait    += wait;
      sum.service += service;
    }
    sum.interarrival = arrival - START;

    DecimalFormat f = new DecimalFormat("###0.00");

    System.out.println("\nfor " + index + " jobs");
    System.out.println("   average interarrival time =   " + f.format(sum.interarrival / index));
    System.out.println("   average wait ............ =   " + f.format(sum.wait / index));
    System.out.println("   average delay ........... =   " + f.format(sum.delay / index));
    System.out.println("   average service time .... =   " + f.format(sum.service / index));
    System.out.println("   average # in the node ... =   " + f.format(sum.wait / departure));
    System.out.println("   average # in the queue .. =   " + f.format(sum.delay / departure));
    System.out.println("   utilization ............. =   " + f.format(sum.service / departure));
  }


   static double exponential(double m, Rng r) {
/* ---------------------------------------------------
 * generate an Exponential random variate, use m > 0.0
 * ---------------------------------------------------
 */
    return (-m * Math.log(1.0 - r.random()));
  }

   double uniform(double a, double b, Rng r) {
/* ------------------------------------------------
 * generate an Uniform random variate, use a < b
 * ------------------------------------------------
 */
    return (a + (b - a) * r.random());
  }


   static double getArrival(Rng r) {
/* ------------------------------
 * generate the next arrival time
 * ------------------------------
 */
//    static double sarrival = START;

    sarrival += exponential(2.0, r);
    return (sarrival);
  }


   double getService(Rng r) {
/* ------------------------------
 * generate the next service time
 * ------------------------------
 */
    return (uniform(1.0, 2.0, r));
  }
    
}
