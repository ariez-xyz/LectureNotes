/* -------------------------------------------------------------------------
 * This is the classic prime sieve of Eratosthenes - a simple algorithm
 * that finds all the prime numbers between 2 and the constant 'N'.
 *
 * Name            : Sieve.java  (Sieve of Eratosthenes)
 * Authors         : Steve Park & Dave Geyer
 * Translated by   : Richard Dutton & Jun Wang
 * Language        : Java
 * Latest Revision : 6-16-06
 * ------------------------------------------------------------------------- */

import java.io.*;
import java.lang.Math;

public class Sieve{

    
    public static void main(String args[])
    {
	final int N = 46341;                /* limited only by memory size   */
	final long SQRTN = ((long) Math.sqrt(N));
	
	int  prime[] = new int[N + 1];      /* n is prime <=> prime[n] == 1  */
	int n;                              /* index of possible primes      */
	long s;                             /* step index                    */
	long t = 0;                         /* index used for tabular output */
	
	prime[0] = 0;                       /* initialize the sieve */
	prime[1] = 0;
	for (n = 2; n <= N; n++)       
	    prime[n] = 1;
	
	for (n = 2; n <= SQRTN; n++)        /* search all possibilities       */
	  if (prime[n] == 1)                /* if n is prime,                 */
	    for (s = 2; s <= (N / n); s++)  /* 2n, 3n, 4n ...  can't be prime */
		    prime[(int)s * n] = 0;
	
	System.out.println("\tThe primes between 2 and " + N + "  are\n");
	for (n = 2; n <= N; n++)
	    if (prime[n] == 1)  {
		if (t == 0)
		    System.out.print("\t");
		System.out.print(n + "  ");
		t = (t + 1) % 10;
		if (t == 0)
		    System.out.println();
	    }
	System.out.println();
	
    }
}
