/* -----------------------------------------------------------------------
 * This program - an extension of program ssq2 - simulates a single-server 
 * machine shop using Exponentially distributed failure times, Uniformly 
 * distributed service times, and a FIFO service queue.  
 *
 * Name            : Ssms.java  (Single Server Machine Shop)
 * Authors         : Steve Park & Dave Geyer
 * Translated by   : Richard Dutton & Jun Wang
 * Language        : Java
 * Latest Revision : 6-16-06
 * -----------------------------------------------------------------------
 */
 
import java.io.*;
import java.lang.Math;
import java.text.DecimalFormat;

public class Ssms{


    final int LAST = 100000;                 /* number of machine failures */
    final double START = 0.0;                /* initial time               */
    final int M = 60;                        /* number of machines         */

    Rngs rngs;
    int m;


    private class Sum{                         /* sum of ...            */
	double wait;                           /*   wait times          */
	double delay;                          /*   delay times         */
	double service;                        /*   service times       */
	double interarrival;                   /*   interarrival times  */
  
	public Sum(){
	    wait = delay = service = interarrival = 0.0;
	}
    } 

    public Ssms(){
	rngs = new Rngs();
    }


    public  double exponential(double m)            
	/* ---------------------------------------------------
	 * generate an Exponential random variate, use m > 0.0 
	 * ---------------------------------------------------
	 */
    {                                       
	return (-m * Math.log(1.0 - rngs.random()));     
    }


    public  double uniform(double a, double b)       
	/* --------------------------------------------
	 * generate a Uniform random variate, use a < b 
	 * --------------------------------------------
	 */
    {                                         
	return (a + (b - a) * rngs.random());    
    }


    public  double getFailure()       
	/* ------------------------------------------------ 
	 * generate the operational time until next failure 
	 * ------------------------------------------------
	 */
    {
	rngs.selectStream(0);
	return (exponential(100.0));
    }


    public  double nextFailure(double failure[])
	/* -----------------------------------------------------------------
	 * return the next failure time, and the index of the failed machine
	 * -----------------------------------------------------------------
	 */
    {
	int    i = 0;
	double t = failure[0];

	m = i;
	for (i = 1; i < M; i++) 
	    if (failure[i] < t) {
		t = failure[i];
		m = i;
	    }
	return (t);
    }


    public  double getService()
	/* ------------------------------
	 * generate the next service time
	 * ------------------------------
	 */ 
    {
	rngs.selectStream(1);
	return (uniform(1.0, 2.0));
    }

    public static void main(String args[]){
	Ssms ssms = new Ssms();
	ssms.run();
    }

    public void run(){

	Sum sum = new Sum();
  
	long   index     = 0;                  /* job (machine failure) index */
	double arrival   = START;              /* time of arrival (failure)   */
	double delay;                          /* delay in repair  queue      */
	double service;                        /* service (repair) time       */
	double wait;                           /* delay + service             */
	double departure = START;              /* time of service completion  */
	double failure[] = new double[M];      /* list of next failure times  */

	rngs.plantSeeds(123456789);

	for (m = 0; m < M; m++)                   /* initial failures */
	    failure[m] = START + getFailure();

	while (index < LAST) {
	    index++;
	    arrival      = nextFailure(failure);
	    if (arrival < departure)
		delay      = departure - arrival;  
	    else
		delay      = 0.0; 
	    service      = getService();
	    wait         = delay + service;
	    departure    = arrival + wait;           /*completion of service  */
	    failure[m]   = departure + getFailure(); /*next failure, machine m*/
	    sum.wait    += wait;
	    sum.delay   += delay;
	    sum.service += service;
	}
	sum.interarrival = arrival - START;

	DecimalFormat df = new DecimalFormat("##0.00");
	System.out.print("\nfor a pool of " + M + " machines "); 
	System.out.println("and " + index + " simulated failures\n");
	System.out.println("average interarrival time .. = " + 
			df.format(sum.interarrival / index));
	System.out.println("average wait ............... = " + 
			df.format(sum.wait / index));
	System.out.println("average delay .............. = " + 
			df.format(sum.delay / index));
	System.out.println("average service time ....... = " + 
			df.format(sum.service / index));
	System.out.println("average # in the node ...... = " + 
			df.format(sum.wait / departure));
	System.out.println("average # in the queue ..... = " + 
			df.format(sum.delay / departure));
	System.out.println("utilization ................ = " + 
			df.format(sum.service / departure));

    }
}
