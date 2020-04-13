
/* -------------------------------------------------------------------------
 * This program simulates a simple (s,S) inventory system using demand read
 * from a text file.  Backlogging is permitted and there is no delivery lag.
 * The output statistics are the average demand and order per time interval
 * (they should be equal), the relative frequency of setup and the time
 * averaged held (+) and short (-) inventory levels.
 *
 * NOTE: use 0 <= MINIMUM < MAXIMUM, i.e., 0 <= s < S.
 *
 * Name              : Sis1.java  (Simple Inventory System, version 1)
 * Authors           : Steve Park & Dave Geyer
 * Translated by     : Jun Wang & Richard Dutton
 * Language          : Java
 * Latest Revision   : 6-16-06
 *
 * Program sis1      : Section 1.3
 * ------------------------------------------------------------------------- 
 */

import java.io.*;
import java.util.*;
import java.text.*;

class Sis1Sum {                                /* sum of ...           */
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

class Sis1 {
  static String FILENAME = "Sis1.dat";         /* input data file */
  static int MINIMUM     = 20;                 /* 's' inventory policy parameter */
  static int MAXIMUM     = 80;                 /* 'S' inventory policy parameter */

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
      long index     = 0;                      /* time interval index     */
      long inventory = MAXIMUM;                /* current inventory level */
      long demand;                             /* amount of demand        */
      long order;                              /* amount of order         */
      Sis1Sum sum = new Sis1Sum();
      sum.initSumParas();

      while ( (line = in.readLine()) != null ) {
        index++;
        st = new StringTokenizer(line);
        if (inventory < MINIMUM) {             /* place an order          */
          order         = MAXIMUM - inventory;
          sum.setup++;
          sum.order    += order;
        }
        else                                   /* no order                 */
          order         = 0;
        inventory      += order;               /* there is no delivery lag */
        demand          = Long.parseLong(st.nextToken());
        sum.demand     += demand;
        if (inventory > demand)
          sum.holding  += (inventory - 0.5 * demand);
        else {
          sum.holding  += (inventory * inventory) / (2.0 * demand);
          sum.shortage += (demand - inventory) * (demand - inventory) / (2.0 * demand);
        }
        inventory      -= demand;
      }

      if (inventory < MAXIMUM) {               /* force the final inventory to */
        order           = MAXIMUM - inventory; /* match the initial inventory  */
        sum.setup++;
        sum.order      += order;
        inventory      += order;
      }

      DecimalFormat f = new DecimalFormat("###0.00");

      System.out.print("\nfor " + index + " time intervals");
      System.out.println(" with an average demand of " + sum.demand / index);
      System.out.println("and policy parameters (s, S) = (" + MINIMUM + ", " + MAXIMUM + ")\n");
      System.out.println("   average order ............ =  " + f.format(sum.order / index));
      System.out.println("   setup frequency .......... =  " + f.format(sum.setup / index));
      System.out.println("   average holding level .... =  " + f.format(sum.holding / index));
      System.out.println("   average shortage level ... =  " + f.format(sum.shortage / index));
    } catch (EOFException eofe) {
      System.out.println("Sis1:" + eofe);
    }
    // if the file opened okay, make sure we close it 
    fis.close();
  }
       
}
