/* ------------------------------------------------------------------------
 * This program illustrates an array-based algorithm for tallying a 
 * continuous-data histogram for data read from standard input (stdin).
 * Like program uvs, a compiled version of this program supports file
 * redirection.
 *
 * The MIN, MAX, K parameters are "tuned" to the datafile uvs.dat.
 * For other datafiles these parameters must be adjusted -- you might
 * want to process the datafile with program uvs first to get a handle on
 * appropriate values for these three parameters.
 * 
 * Name              : Cdh.java  (Continuous Data Histogram)
 * Authors           : Steve Park & Dave Geyer
 * Translated by     : Jun Wang
 * Language          : Java
 * Latest Revision   : 6-16-06
 * ------------------------------------------------------------------------ 
 */

import java.io.*;
import java.util.*;
import java.text.*;


class Outliers {
  long lo;
  long hi;
}

class Cdh {

  static double MIN   = 0.0;
  static double MAX   = 8.0;
  static int    K     = 16;                        /* number of histogram bins   */
  static double DELTA = ((MAX - MIN) / K);         /* histogram bin size (width) */

  public static void main(String[] args) throws IOException {

    double x;                                      /* data value          */
    int    j;                                      /* histogram bin index */
    long   index    = 0;                           /* sample size         */
    long   count[] = new long [K];                 /* bin count           */
    double midpoint[] = new double [K];            /* bin midpoint        */
    double sum      = 0.0;
    double sumsqr   = 0.0;
    double mean;
    double stdev;
    Outliers o = new Outliers();
    o.lo = 0;
    o.hi = 0;

    for (j = 0; j < K; j++) {
      count[j] = 0;
      midpoint[j] = MIN + (j + 0.5) * DELTA;
    }

    String line = null;
    InputStreamReader r = new InputStreamReader(System.in);
    BufferedReader in = new BufferedReader(r);
    try {
      while ( (line = in.readLine()) != null ) {
        x = Double.parseDouble(line);
        index++;
        if ((x >= MIN) && (x < MAX)) {
          j = (int) ((x - MIN) / DELTA);
          count[j]++;
        }
        else if (x < MIN)
          o.lo++;
        else
          o.hi++;
      }
    } catch (EOFException e) {
      System.out.println("Cdh: " + e);
    } catch (NumberFormatException nfe) {
//      System.out.println("Cdh: " + nfe);
    }

    if (index > 0) {
      for (j = 0; j < K; j++)                        /* histogram mean */
        sum += midpoint[j] * count[j];
      mean   = sum / index;

      for (j = 0; j < K; j++)                        /* histogram stdev */
        sumsqr += Math.pow((midpoint[j] - mean), 2) * count[j];
      stdev     = Math.sqrt(sumsqr / index);

      DecimalFormat f = new DecimalFormat("###0.000");

      System.out.println("bin\tmidpoint\tcount\tproportion\tdensity\n");
      for (j = 0; j < K; j++) { 
        System.out.print((j + 1) + "\t");                         /* bin        */
        System.out.print(f.format(midpoint[j]) + "\t\t");                   /* midpoint   */
        System.out.print(count[j] + "\t");                        /* count      */
        System.out.print(f.format((double) count[j] / index) + "\t\t");     /* proportion */
        System.out.println(f.format((double) count[j] / (index * DELTA)));  /* density    */
      }
      System.out.println("\nsample size .... = " + index);
      System.out.println("mean ........... = " + f.format(mean));
      System.out.println("stdev .......... = " + f.format(stdev) + "\n");
      if (o.lo > 0)
        System.out.println("NOTE: there were " + o.lo + " low outliers");
      if (o.hi > 0)
        System.out.println("NOTE: there were " + o.hi + " high outliers");
    }
  }
}
