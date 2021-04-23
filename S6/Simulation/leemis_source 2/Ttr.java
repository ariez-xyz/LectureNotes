
/* ------------------------------------------------------------------------- 
 * This program is a next-event simulation of a multi-user time-sharing
 * system.  All users begin with the thinking task and the simulation ends 
 * when the simulation clock time meets or exceeds the terminal time STOP.
 * 
 * Name              : Ttr.java (Think-Type-Receive)
 * Author            : Larry Leemis 
 * Translated by     : Richard Dutton
 * Language          : Java 
 * Latest Revision   : 6-16-06 
 * ------------------------------------------------------------------------- 
 */

import java.io.*;
import java.lang.Math;
import java.text.*;

class Event{
    double time;                     /* event time                  */
    int    type;                     /* event type                  */
    long   info;                     /* ancillary information       */

    public Event(){
	// time = 0.0;
	// type = 0;
	// info = 0;
    }	
}

public class Ttr{
    
    static final double START = 0.0;     /* initial simulation clock time  */
    static final double STOP = 100.0;    /* terminal time                  */
    static final int N = 5;              /* number of servers              */

    static Rngs rngs;
    
    static public long equilikely(long a, long b)
	/* ===================================================================
	 * Returns an equilikely distributed integer between a and b inclusive.
	 * NOTE: use a < b
	 * ===================================================================
	 */
    {
	return (a + (long) ((b - a + 1) * rngs.random()));
    }

    static public double uniform(double a, double b)
	/* =========================================================== 
	 * Returns a uniformly distributed real number between a and b. 
	 * NOTE: use a < b
	 * ===========================================================
	 */
    { 
	return (a + (b - a) * rngs.random());
    }
   

    static public double getThinkTime()
	/* ----------------------------
	 * generate the next think time 
	 * ----------------------------
	 */ 
    {     
	rngs.selectStream(0);
	return (uniform(0.0, 10.0));
    }

   
    static public double getKeystrokeTime()
	/* -------------------------------
	 * generate the next keystroke time 
	 * -------------------------------
	 */ 
    {     
	rngs.selectStream(1);
	return (uniform(0.15, 0.35));
    }

   
    static public long getNumKeystrokes()
	/* ---------------------------------
	 * generate the number of keystrokes 
	 * ---------------------------------
	 */ 
    {     
	rngs.selectStream(2);
	return (equilikely(5, 15));
    }

   
    static public long getNumCharacters()
	/* ---------------------------------
	 * generate the number of characters 
	 * ---------------------------------
	 */ 
    {     
	rngs.selectStream(3);
	return (equilikely(50, 300));
    }

    public static void main(String args[]){
        rngs = new Rngs();
        
	int i;                    /* loop parameter                         */
        int j = -1;               /* index for the next event               */
	long nevents = 0;         /* number of events during the simulation */
	long nsearches = 0;       /* number of event list searches          */
	double tnow;              /* simulation clock                       */
	double temp;              /* used to find time of next event        */
	double ReceiveRate = 1.0 / 120.0;  /* time to transmit a character  */
        
        Event event[] = new Event[N];
        for(i=0;i<N;i++){
            event[i] = new Event();
        }

	rngs.plantSeeds(0);
	tnow = START;
	for (i = 0; i < N; i++) {
	    event[i].time = getThinkTime();
	    event[i].type = 1;
	    event[i].info = 0;
	}

	while (tnow < STOP) { 
	    nevents++;
	    temp = 100.0 * STOP;
	    for (i = 0; i < N; i++) {
		nsearches++;
		if (event[i].time <= temp) {
		    temp = event[i].time;
		    j = i;
		}
	    }
	    tnow = event[j].time;   
	    if (event[j].type == 1) {           /* complete thinking event  */
		event[j].time = tnow + getKeystrokeTime();
		event[j].type = 2;
		event[j].info = getNumKeystrokes();
	    }
	    else if (event[j].type == 2) {      /* complete keystroke event */
		event[j].info--; 
		if (event[j].info > 0) {
		    event[j].time = tnow + getKeystrokeTime();
		}
		else {                            /* last keystroke          */
		    event[j].time = tnow + ReceiveRate; 
		    event[j].type = 3;
		    event[j].info = getNumCharacters();
		}
	    }
	    else if (event[j].type == 3) {      /* complete character recpt */
		event[j].info--; 
		if (event[j].info > 0) {
		    event[j].time = tnow + ReceiveRate; 
		}
		else {                            /* last character          */
		    event[j].time = tnow + getThinkTime(); 
		    event[j].type = 1;
		    event[j].info = 0;
		}
	    }
	    else {
		//printf("\nerror: event type must be 1, 2, or 3\n");  
		System.out.println("\nerror: event type must be 1, 2, or 3");  
	    }
	} 

        DecimalFormat f2 = new DecimalFormat("###0.00");
        DecimalFormat f3 = new DecimalFormat("###0.000");
        
	//printf("\nsimulation end time .... = %6.2f\n", tnow);
	System.out.println("\nsimulation end time .... = " + f2.format(tnow));
	//printf("\nfinal status of the event list:\n");  
	System.out.println("\nfinal status of the event list:");  
	for (i = 0; i < N; i++){
	    //printf("%8d %14.3f %8d %8d\n", i, event[i].time, event[i].type, event[i].info); 
	    System.out.println("\t" + i + "\t" + f3.format(event[i].time) + 
			       "\t\t" + event[i].type + "\t" + event[i].info); 
        }
        
	//printf("\nnumber of events ....... = %ld\n", nevents);
	System.out.println("\nnumber of events ....... = " + nevents);
	//printf("\nnumber of searches ..... = %ld\n", nsearches);
	System.out.println("\nnumber of searches ..... = " + nsearches);
	//printf("\n");
	System.out.println();

    }
}
    
