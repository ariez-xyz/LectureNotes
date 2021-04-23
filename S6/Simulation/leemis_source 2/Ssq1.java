
/* -------------------------------------------------------------------------
 * This program simulates a single-server FIFO service node using arrival
 * times and service times read from a text file.  The server is assumed
 * to be idle when the first job arrives.  All jobs are processed completely
 * so that the server is again idle at the end of the simulation.   The
 * output statistics are the average interarrival time, average service
 * time, the average delay in the queue, and the average wait in the service 
 * node. 
 *
 * Name              : Ssq1.java  (Single Server Queue, version 1)
 * Authors           : Steve Park & Dave Geyer
 * Translated by     : Jun Wang & Richard Dutton
 * Language          : Java
 * Latest Revision   : 6-16-06
 * Program ssq1      : Section 1.2
 * ------------------------------------------------------------------------- 
 */

import java.io.*;
import java.util.*;
import java.text.*;

class Ssq1Sum {                                 /* sum of ...           */
  double delay;                                 /*   delay times        */
  double wait;                                  /*   wait times         */
  double service;                               /*   service times      */
  double interarrival;                          /*   interarrival times */

  void initSumParas() {
    delay = 0.0;
    wait = 0.0;
    service = 0.0;
    interarrival = 0.0;
  }
}

class Ssq1 {
  static String FILENAME = "Ssq1.dat";          /* input data file */
  static double START = 0.0;

  public static void main(String[] args) throws IOException {
      
    FileInputStream fis = null;
    try {
      fis = new FileInputStream(FILENAME);
    } catch (FileNotFoundException fnfe) {
      System.out.println("Cannot open input file" + FILENAME);
      System.exit(1);
    }
      
    InputStreamReader r = new InputStreamReader(fis);
    BufferedReader in = new BufferedReader(r);
    try {
      String line = null;
      StringTokenizer st = null;
      long   index     = 0;                     /* job index            */
      double arrival   = START;                 /* arrival time         */
      double delay;                             /* delay in queue       */
      double service;                           /* service time         */
      double wait;                              /* delay + service      */
      double departure = START;                 /* departure time       */
      Ssq1Sum sum = new Ssq1Sum();
      sum.initSumParas();

      while ( (line = in.readLine()) != null ) {
        index++;
        st = new StringTokenizer(line);
        arrival = Double.parseDouble(st.nextToken());
        if (arrival < departure)
          delay    = departure - arrival;       /* delay in queue    */
        else
          delay    = 0.0;                       /* no delay          */
        service = Double.parseDouble(st.nextToken());
        wait         = delay + service;
        departure    = arrival + wait;          /* time of departure */
        sum.delay   += delay;
        sum.wait    += wait;
        sum.service += service;
      }
      sum.interarrival = arrival - START;

      DecimalFormat f = new DecimalFormat("###0.00");

      System.out.println("\nfor " + index + " jobs");
      System.out.println("   average interarrival time =  " + f.format(sum.interarrival / index));
      System.out.println("   average service time .... =  " + f.format(sum.service / index));
      System.out.println("   average delay ........... =  " + f.format(sum.delay / index));
      System.out.println("   average wait ............ =  " + f.format(sum.wait / index));

    } catch (EOFException eofe) {
      System.out.println("Ssq1:" + eofe);
    }
    // if the file opened okay, make sure we close it 
    fis.close();
  }
    
}
