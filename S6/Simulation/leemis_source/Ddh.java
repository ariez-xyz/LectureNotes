/* ------------------------------------------------------------------------- 
 * This program illustrates a linked-list algorithm for tallying a 
 * discrete-data histogram for data read from standard input (stdin).   
 * Like program uvs, a compiled version of this program supports file 
 * redirection. 
 * 
 * NOTE: if the data is not discrete, i.e., virtually all inputs are likely 
 * to be unique, and if the number of inputs is large (say 10,000 or more)
 * then this program will execute for a LOOOONG time!
 *
 * Name              : Ddh.java  (Discrete Data Histogram)
 * Authors           : Steve Park & Dave Geyer
 * Translated by     : Jun Wang & Robin Givens
 * Language          : Java
 * Latest Revision   : 7-29-06
 *
 * Program ddh       : Section 4.2, based on Algorithm 4.2.2
 * ------------------------------------------------------------------------- 
 */

import java.io.*;
import java.util.*;
import java.text.*;

class DdhNode implements Comparable<DdhNode> {
  double value;
  long   count;

  void init(double data) {
    value = data;
    count = 1;
  }
  
  void inc() {
    count++;
  }
  
  boolean equals(double data) {
    return (value == data);
  }
  
  public int compareTo(DdhNode o) {
    if (value > o.value)
      return 1;
    else if (value == o.value)
      return 0;
    else
      return -1;
  }
};


class Ddh {

  public static void main(String[] args) throws IOException {

    double  data;
    DdhNode head = null;
    Ddh     d    = new Ddh();
    List<DdhNode> ddhlist = new ArrayList<DdhNode>();

    String line;
    InputStreamReader r = new InputStreamReader(System.in);
    BufferedReader ReadThis = new BufferedReader(r);
    try {
      if ( (line = ReadThis.readLine()) != null) {
        data = Double.parseDouble(line);
        head = new DdhNode();
        head.init(data);
        ddhlist.add(head);       /* the head of the list */
      }

      while ( (line = ReadThis.readLine()) != null ) {
        data = Double.parseDouble(line);
        d.insert(ddhlist, data);
      }
    } catch (EOFException e) {
      System.out.println("Ddh: " + e);
    } catch (NumberFormatException nfe) {
//      System.out.println("Ddh: " + nfe);
    }

    if (head != null) {
      d.sort(ddhlist);
      d.traverse(ddhlist);
    }
  }

  void insert(List<DdhNode> dl, double data) {
/* --------------------------------------
 * add 'data' to the list
 * --------------------------------------
 */
    boolean found = false;
    DdhNode tmp;
    
    Iterator<DdhNode> iter = dl.iterator();

    while (iter.hasNext() && !found) {
      tmp = (DdhNode)iter.next();
      if (tmp.equals(data)) {     /* if 'data' was found         */
        tmp.inc();               /* inc the corresponding count */
        found = true;
      }
    }

    if (!found) {                           
      tmp = new DdhNode();
      tmp.init(data);
      dl.add(tmp);              /* add a new node (at the end) */
    }
  }


  void traverse(List<DdhNode> dl) {
/* ---------------------------------------------------------------
 * traverse the list to compute the histogram statistics and print
 * the histogram
 * ---------------------------------------------------------------
 */
    DdhNode  p;
    long     index  = 0;
    double   sum    = 0.0;
    double   sumsqr = 0.0;
    double   diff;
    double   mean;
    double   stdev;
    
    Iterator<DdhNode> iter = dl.iterator();
    while (iter.hasNext()) {            /* traverse the list               */
      p = (DdhNode)iter.next();
      index += p.count;                /* to accumulate 'sum' and 'index' */
      sum   += p.value * p.count;
    }
    mean = sum / index;

    iter = dl.iterator();
    while (iter.hasNext()) {            /* traverse the list               */
      p = (DdhNode)iter.next();
      diff    = p.value - mean;
      sumsqr += diff * diff * p.count;
    }
 
    stdev = Math.sqrt(sumsqr / index);
    System.out.println("value\tcount\tproportion\n");
    iter = dl.iterator();

    DecimalFormat f = new DecimalFormat("###0.000");

    while (iter.hasNext()) {            /* traverse the list               */
      p = (DdhNode)iter.next();         /* to print the histogram */
      System.out.println(p.value + "\t" + p.count +
        "\t" + f.format(((double) p.count / index)));
    }
    System.out.println("\nsample size ........... = " + index);
    System.out.println("mean .................. = " + f.format(mean));
    System.out.println("standard deviation .... = " + f.format(stdev));
  }

  
  void sort(List<DdhNode> dl) {
/* -------------------------------
 * selection sort - slow, but sure
 * -------------------------------
 */
    Collections.sort(dl);
  }

}
