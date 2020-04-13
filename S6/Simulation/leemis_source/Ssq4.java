/* ------------------------------------------------------------------------- 
 * This program is a next-event simulation of a single-server FIFO service
 * node using Exponentially distributed interarrival times and Erlang 
 * distributed service times (i.e., a M/E/1 queue).  The service node is 
 * assumed to be initially idle, no arrivals are permitted after the 
 * terminal time STOP, and the node is then purged by processing any 
 * remaining jobs in the service node.
 *
 * Name            : Ssq4.java  (Single Server Queue, version 4)
 * Authors         : Steve Park & Dave Geyer
 * Translated by   : Richard Dutton & Jun Wang
 * Language        : Java
 * Latest Revision : 6-16-06
 * ------------------------------------------------------------------------- 
 */

import java.io.*;
import java.lang.Math;
import java.text.*;

public class Ssq4{
    final double START = 0.0;               /*initial time                   */
    final double STOP = 20000.0;            /*terminal (close the door) time */
    final double INFINITY = (100.0 * STOP); /*must be much larger than STOP  */
    Rngs rngs;
    Rvgs rvgs;
    double arrival = START;

    private class T{
	double arrival;                /* next arrival time                   */
	double completion;             /* next completion time                */
	double current;                /* current time                        */
	double next;                   /* next (most imminent) event time     */
	double last;                   /* last arrival time                   */

	public T(){}
    }
    private class Area {
	double node;                   /* time integrated number in the node  */
	double queue;                  /* time integrated number in the queue */
	double service;                /* time integrated number in service   */
   
	public Area(){
	    node = queue = service = 0.0;
	}
    }

    public Ssq4(){
    rngs = new Rngs();
  	rvgs = new Rvgs(rngs);
    }

    public double min(double a, double c)
	/* ------------------------------
	 * return the smaller of a, b
	 * ------------------------------
	 */
    { 
	if (a < c)
	    return (a);
	else
	    return (c);
    } 


    public double getArrival()
	/* ---------------------------------------------
	 * generate the next arrival time, with rate 1/2
	 * ---------------------------------------------
	 */ 
    {
	//arrival = START;
  
	rvgs.rngs.selectStream(0); 
	arrival += rvgs.exponential(2.0);
	return (arrival);
    } 


    public double getService()
	/* --------------------------------------------
	 * generate the next service time with rate 2/3
	 * --------------------------------------------
	 */ 
    {
	rvgs.rngs.selectStream(1);
	return (rvgs.erlang(5, 0.3));
    }  


    public static void main(String args[])
    {
	Ssq4 ssq4 = new Ssq4(); 
	ssq4.run();
    }

    public void run(){
	T t = new T();
	Area area = new Area();
  
	long index  = 0;               /* used to count departed jobs         */
	long number = 0;               /* number in the node                  */

	rvgs.rngs.plantSeeds(0);
	t.current    = START;        /* set the clock                         */
	t.arrival    = getArrival(); /* schedule the first arrival            */
  
	t.completion = INFINITY;     /* the first event can't be a completion */
	while ((t.arrival < STOP) || (number > 0)) {
	    t.next          = min(t.arrival, t.completion); /*next event time */
	    if (number > 0)  {                              /*update integrals*/
		area.node    += (t.next - t.current) * number;
		area.queue   += (t.next - t.current) * (number - 1);
		area.service += (t.next - t.current);
	    }
	    t.current       = t.next;                   /* advance the clock */

	    if (t.current == t.arrival)  {              /* process an arrival*/
		number++;
		t.arrival     = getArrival();
		if (t.arrival > STOP)  {
		    t.last      = t.current;
		    t.arrival   = INFINITY;
		}
		if (number == 1)
		    t.completion = t.current + getService();
	    }

	    else {                                    /* process a completion */
		index++;
		number--;
		if (number > 0)
		    t.completion = t.current + getService();
		else
		    t.completion = INFINITY;
	    }
	} 

	DecimalFormat f = new DecimalFormat("###0.00");
  
	System.out.println("\nfor " + index + " jobs");
	System.out.println("   average interarrival time =   " + 
			f.format(t.last / index));
	System.out.println("   average wait ............ =   " + 
			f.format(area.node / index));
	System.out.println("   average delay ........... =   " + 
			f.format(area.queue / index));
	System.out.println("   average service time .... =   " + 
			f.format(area.service / index));
	System.out.println("   average # in the node ... =   " + 
			f.format(area.node / t.current));
	System.out.println("   average # in the queue .. =   " + 
			f.format(area.queue / t.current));
	System.out.println("   utilization ............. =   " + 
			f.format(area.service / t.current));

    }
}
