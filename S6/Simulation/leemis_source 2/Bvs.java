/* -------------------------------------------------------------------------
 * This program reads bivariate data from a text file and computes the mean,
 * standard deviation, minimum, maximum, correlation coefficient and
 * regression line angle (theta).
 *
 * NOTE: the text data file is assumed to be in a two-values-per-line format
 * (i.e. bivariate format) with NO blank lines in the file.
 *
 * NOTE: for more information relative to the use of this program, see the
 * header of the analogous univariate program uvs.c
 *
 * Name              : Bvs.java (BiVariate Statistics)
 * Authors           : Steve Park & Dave Geyer
 * Translated by     : Jun Wang
 * Language          : Java
 * Latest Revision   : 6-16-06
 * -------------------------------------------------------------------------
 */

import java.io.*;
import java.util.*;
import java.text.*;

class Bivariate {
  double u;
  double v;
  
  void initSumParas() {
    u   = 0.0;
    v   = 0.0;
  }
}

class Bvs {

  public static void main(String[] args) throws IOException {

    long      index = 0;
    double    cosum = 0.0;
    double    covariance;
    double    correlation;
    double    temp;
    double    theta;
    double    pi = Math.PI;        /* 3.14159 ... */
    Bivariate data  = new Bivariate();
    Bivariate sum   = new Bivariate();
    Bivariate mean  = new Bivariate();
    Bivariate stdev = new Bivariate();
    Bivariate min   = new Bivariate();
    Bivariate max   = new Bivariate();
    Bivariate diff  = new Bivariate();
    data.initSumParas();
    sum.initSumParas();
    mean.initSumParas();
    stdev.initSumParas();
    min.initSumParas();
    max.initSumParas();
    diff.initSumParas();

    InputStreamReader r = new InputStreamReader(System.in);
    BufferedReader in = new BufferedReader(r);
    try {
      String line = null;
      StringTokenizer st = null;

      if ( (line = in.readLine()) != null) {
        st = new StringTokenizer(line);
        data.u = Double.parseDouble(st.nextToken());
        data.v = Double.parseDouble(st.nextToken());
        index  = 1;
        mean.u = data.u;
        mean.v = data.v;
        min.u  = data.u;
        min.v  = data.v;
        max.u  = data.u;
        max.v  = data.v;
      }
//      else
//        index = 0;

      while ( (line = in.readLine()) != null ) {
        st      = new StringTokenizer(line);
        data.u  = Double.parseDouble(st.nextToken());
        data.v  = Double.parseDouble(st.nextToken());
        index++;
        temp    = (index - 1.0) / index;
        diff.u  = data.u - mean.u;
        diff.v  = data.v - mean.v;
        sum.u  += diff.u * diff.u * temp;
        sum.v  += diff.v * diff.v * temp;
        cosum  += diff.u * diff.v * temp;
        mean.u += diff.u / index;
        mean.v += diff.v / index;
        if (data.u > max.u)
          max.u = data.u;
        else if (data.u < min.u)
          min.u = data.u;
        if (data.v > max.v)
          max.v = data.v;
        else if (data.v < min.v)
          min.v = data.v;
      }
    } catch (EOFException e) {
      System.out.println("Bvs: " + e);
    } catch (NumberFormatException nfe) {
//      System.out.println("Bvs: " + nfe);
    } catch (NoSuchElementException nsee) {
//      System.out.println("Bvs: " + nsee);
    }
      
    if (index > 0) {
      stdev.u    = Math.sqrt(sum.u / index);
      stdev.v    = Math.sqrt(sum.v / index);
      covariance = cosum / index;
      if (stdev.u * stdev.v > 0.0)
        correlation = covariance / (stdev.u * stdev.v);
      else
        correlation = 0.0;
      sum.u = stdev.u * stdev.u - stdev.v * stdev.v;
      sum.v = 2.0 * covariance;
      theta = 0.5 * Math.atan2(sum.v, sum.u);

      DecimalFormat f = new DecimalFormat("#####0.000");
        
      System.out.println("\nfor a bivariate sample of size " + index);
      System.out.println("\nmean.u ...... = " +  f.format(mean.u));
      System.out.println("stdev.u ..... = " + f.format(stdev.u));
      System.out.println("min.u ....... = " + f.format(min.u));
      System.out.println("max.u ....... = " + f.format(max.u));
      System.out.println("\nmean.v ...... = " + f.format(mean.v));
      System.out.println("stdev.v ..... = " + f.format(stdev.v));
      System.out.println("min.v ....... = " + f.format(min.v));
      System.out.println("max.v ....... = " + f.format(max.v));
      System.out.println("\ncorrelation ...... = " + f.format(correlation));
      System.out.println("theta (radians) .. = " + f.format(theta));
      System.out.println("theta (degrees) .. = " + f.format(180.0 * theta / pi));
    }
  }
}
