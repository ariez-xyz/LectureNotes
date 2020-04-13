/* -------------------------------------------------------------------------  
 * This program reads a (text) data file and computes the mean, minimum, 
 * maximum, and standard deviation.   The one-pass algorithm used is due to 
 * B.P. Welford (Technometrics, vol. 4, no 3, August 1962.) 
 *
 * NOTE: the text data file is assumed to be in a one-value-per-line format
 * with NO blank lines in the file.   The data can be either fixed point 
 * (integer valued), or floating point (real valued). 
 *
 * To use the program, compile it to disk to produce uvs.  Then at a command 
 * line prompt, uvs can be used in three ways. 
 *
 * (1) To have uvs read a disk data file, say uvs.dat (in the format above),
 * at a command line prompt use '<' redirection as: 
 *
 *     uvs < uvs.dat
 *
 * (2) To have uvs filter the numerical output of a program, say test, at a
 * command line prompt use '|' pipe as: 
 *
 *     test | uvs
 *
 * (3) To use uvs with keyboard input, at a command line prompt enter:
 *
 *      uvs
 *
 * Then enter the data -- one value per line -- being sure to remember to
 * signify an end-of-file.  In Unix/Linux, signify an end-of-file by
 * entering ^d (Ctrl-d) as the last line of input.
 * 
 * Name              : Uvs.java (Multi-Server Queue)
 * Authors           : Steve Park & Dave Geyer
 * Translated by     : Jun Wang
 * Language          : Java
 * Latest Revision   : 6-16-06
 *
 * Program uvs       : Section 4.1, based on Algorithm 4.1.1
 * -------------------------------------------------------------------------
 */

import java.io.*;
import java.util.*;
import java.text.*;


class Uvs {

  public static void main(String[] args) throws IOException {

    long    index;
    double  data;
    double  sum = 0.0;
    double  mean;
    double  stdev;
    double  min;
    double  max;
    double  diff;

    String line;
    InputStreamReader r = new InputStreamReader(System.in);
    BufferedReader ReadThis = new BufferedReader(r);
    index = 0;
    mean  = 0.0;
    min   = 0.0;
    max   = 0.0;
    
    try {
      if ( (line = ReadThis.readLine()) != null) {
        data  = Double.parseDouble(line);
        index = 1;
        mean  = data;
        min   = data;
        max   = data;
      }

      while ( (line = ReadThis.readLine()) != null ) {
        data  = Double.parseDouble(line);
        index++;
        diff  = data - mean;
        sum  += diff * diff * (index - 1.0) / index;
        mean += diff / index;
        if (data > max)
          max = data;
        else if (data < min)
          min = data;
      }
    } catch (EOFException e) {
      System.out.println("Uvs: " + e);
    } catch (NumberFormatException nfe) {
//      System.out.println("Uvs: " + nfe);
    }

    if (index > 0) {
      DecimalFormat f = new DecimalFormat("###0.000");
      stdev = Math.sqrt(sum / index);
      System.out.println("\nfor a sample of size " + index);
      System.out.println("mean ................. =  " + f.format(mean));
      System.out.println("standard deviation ... =  " + f.format(stdev));
      System.out.println("minimum .............. =  " + f.format(min));
      System.out.println("maximum .............. =  " + f.format(max));
    }
  }
}
