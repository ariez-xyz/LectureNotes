
/* ----------------------------------------------------------------------  
 * A Monte Carlo simulation of the hat check girl problem.
 *
 * Name              : Hat.java
 * Authors           : Steve Park & Dave Geyer
 * Translated by     : Richard Dutton
 * Language          : Java
 * Latest Revision   : 6-16-06
 * ---------------------------------------------------------------------- */

import java.io.*;
import java.text.*;
import java.text.*;

public class Hat{
    static final int SIZE = 10;            /* array size                */
    static final long N = 10000;           /* number of replications    */


    //typedef long permutation[SIZE];

    /* global variables */
    static long i;                              /* replication index         */
    static long arr[] = new long[SIZE];         /* array                     */
    static long count = 0;                      /* # of times a match occurs */
    static double p;                            /* probability estimate      */

    static Rng rng;

    /* =============================== */
    static long equilikely(long a, long b)            /* use a < b  */
    /* =============================== */
    {
	return(a + (long) ((b - a + 1) * rng.random()));
    }

    /* ============================== */
    static void initialize(long[] a)
    /* ============================== */
    {
	int j;

	for(j = 0; j < SIZE; j++)
	    a[j] = j;
    }

    /* =========================== */
    static void shuffle(long[] a)
    /* =========================== */
    { 
	int j;
	long t;
	long hold;

	for(j = 0; j < (SIZE - 1); j++) {         /* shuffle an array       */
	    t     = equilikely(j, (SIZE - 1));    /* in such a way that all  */
	    hold  = a[j];                         /* permutations are equally*/
	    a[j]  = a[(int)t];                    /* likely to occur        */
	    a[(int)t]  = hold;
	}
    }

    /* ============================ */
    static int check(long[] a)
    /* ============================ */
    { 
	int j    = 0;
	boolean test = false;

	do {                                   /* test to see if at least */
	    test = (a[j] == j);                /* one element is in its   */
	    j++;                               /* 'natural' position      */
	} while ((j != SIZE) && !test);        /* - return a 1 if so      */
	return (test ? 1 : 0);                 /* - return a 0 otherwise  */
    }

    /* ============== */
    public static void main(String args[]){
    /* ============== */

	rng = new Rng();
	rng.putSeed(0);

	initialize(arr);

	for(i = 0; i < N; i++) {            /* do N Monte Carlo replications */
	    shuffle(arr);
	    count += check(arr);
	}

	p = (double) (N - count) / (double) N;   /* estimate the probability */

  DecimalFormat f = new DecimalFormat("###0.000");

	//printf("\nfor %ld replications and an array of size %d\n", N, SIZE);
	System.out.println("\nfor " + N 
			   + " replications and an array of size " + SIZE);
	//printf("the estimated probability is %5.3lf\n\n", p);
	System.out.println("the estimated probability is " + f.format(p) + "\n");

    }
}
