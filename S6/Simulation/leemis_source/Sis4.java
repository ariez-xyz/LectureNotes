/* -------------------------------------------------------------------------  
 * This program is a next-event simulation of a simple inventory system with 
 * a Poisson/Geometric demand model where the demand instances are generated
 * as a Poisson process and at each demand instance the demand amount is a
 * Geometric random variate.  Backlogging is possible and when inventory is
 * ordered there is aa Uniform(0,1) delivery lag. 
 *
 * Name              : Sis4.java  (Simple Inventory System, version 4) 
 * Authors           : Steve Park & Dave Geyer 
 * Translated by     : Richard Dutton & Jun Wang
 * Language          : Java
 * Latest Revision   : 6-16-06
 * ------------------------------------------------------------------------- 
 */

import java.io.*;
import java.lang.Math;
import java.text.DecimalFormat;

public class Sis4{
    final long MINIMUM = 20;           /* 's' inventory policy parameter >= 0 */
    final long MAXIMUM = 80;           /* 'S' inventory policy parameter > s  */
    final double START = 0.0;
    final double STOP = 100.0;
    final double INFINITY = (100.0 * STOP);
 
    double time;
    Rngs rngs;
    Rvgs rvgs;
    long amount;
  
    private class Sum{                           /* sum of -                */
	double setup = 0.0;                      /*   setup instances       */
	double holding = 0.0;                    /*   inventory held (+)    */
	double shortage = 0.0;                   /*   inventory short (-)   */
	double order = 0.0;                      /*   orders                */
	double demand = 0.0;                     /*   demands               */
	double lag = 0.0;                        /*   lags                  */
    }

    private class T {
	double demand;                     /* next demand time                */
	double review;                     /* next inventory review time      */
	double arrive;                     /* next order arrival time         */
	double current;                    /* current time                    */
	double next;                       /* next (most imminent) event time */
    } 

    public Sis4(){
 	time = START;
    }

    public double getDemand()
	/* ------------------------------------------
	 * generate a demand instance with rate 120
	 * and generate a corresponding demand amount
	 * ------------------------------------------
	 */
    {             
	//static double time = START;
                            
	rngs.selectStream(0);
	time += rvgs.exponential(1.0 / 120.0);      /* demand instance */
	rngs.selectStream(2);
	amount = rvgs.geometric(0.2);               /* demand amount   */
	return (time);
    }


    public double getLag()
	/* ----------------------- 
	 * generate a delivery lag
	 * -----------------------
	 */
    {                                         
	rngs.selectStream(1);
	return (rvgs.uniform(0.0, 1.0));
    }


    public double min(double a, double b, double c)    
	/* ----------------------------------------
	 * return the minimum of a, b, c
	 * ----------------------------------------
	 */
    {
	double t = a;

	if (b < t)
	    t = b;
	if (c < t)
	    t = c;
	return (t);
    }

    public static void main(String[] args)
    {
  	Sis4 sis4 = new Sis4();
  	sis4.run();
    }

    public void run(){
	rngs = new Rngs();
	rvgs = new Rvgs(rngs);
	long inventory = MAXIMUM;            /* current inventory level      */
	long order     = 0;                  /* current order level          */
	T t = new T();
  
	Sum sum = new Sum();

	rngs.plantSeeds(0);
	t.current = START;
	t.demand  = getDemand();             /* schedule the first demand */
	t.review  = t.current + 1.0;         /* schedule the first review */
	t.arrive  = INFINITY;                /* no order arrival pending  */

	while (t.current < STOP) {
	    t.next          = min(t.demand, t.review, t.arrive);
	    if (inventory > 0)
		sum.holding  += (t.next - t.current) * inventory;
	    else
		sum.shortage -= (t.next - t.current) * inventory;
	    t.current       = t.next;

	    if (t.current == t.demand) {     /* process an inventory demand */
		sum.demand += amount;
		inventory  -= amount;
		t.demand    = getDemand();
	    }

	    else if (t.current == t.review) {   /* process inventory review */
		if (inventory < MINIMUM) {
		    double lag;
		    order      = MAXIMUM - inventory;
		    lag        = getLag();
		    sum.setup++;
		    sum.order += order;
		    sum.lag   += lag;
		    t.arrive   = t.current + lag;
		}
		t.review  = t.current + 1.0;
	    }

	    else {                      /* process an inventory order arrival*/
		inventory += order;
		order      = 0;
		t.arrive   = INFINITY;
	    }
	} 

	if (inventory < MAXIMUM) {      /* adjust the final inventory level */
	    order      = MAXIMUM - inventory;
	    sum.setup++;
	    sum.order += order;
	    inventory += order;
	}

	DecimalFormat df = new DecimalFormat("##0.00");

	System.out.print("\nfor " + (long)STOP + " time intervals");
	System.out.println(" with an average demand of " + 
			df.format(sum.demand / STOP));
	if (sum.setup > 0.0){
	    System.out.print("an average lag of " + 
			    df.format(sum.lag / sum.setup));
	}
	System.out.println(" and policy parameters (s, S) = (" + MINIMUM + 
			", " + MAXIMUM + ")\n");
	System.out.println("   average order ............ =  " + 
			df.format(sum.order / STOP));
	System.out.println("   setup frequency .......... =  " + 
			df.format(sum.setup / STOP));
	System.out.println("   average holding level .... =  " + 
			df.format(sum.holding / STOP));
	System.out.println("   average shortage level ... =  " + 
		df.format(sum.shortage / STOP) + "\n");

    }

}
