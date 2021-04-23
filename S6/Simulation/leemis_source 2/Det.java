
/* ---------------------------------------------------------------------- *
 * A Monte Carlo simulation to estimate the probability that the
 * determinant of a 3 by 3 matrix of random numbers is positive.
 *
 * Name              : Det.java
 * Author            : Larry Leemis
 * Translated by     : Richard Dutton
 * Language          : Java
 * Latest Revision   : 6-16-06
 * ---------------------------------------------------------------------- */

import java.io.*;
import java.text.*;

public class Det{
    static long N = 200000000;                  /* number of replications    */
    static Rng rng;
    public static void main(String args[]){

	rng = new Rng();
	int    i;                              /* replication index         */
	int    j;                              /* row index                 */
	int    k;                              /* column index              */
	double a[][] = new double[4][4];       /* matrix(9 elements used)   */
	double temp1;                          /* first 2 by 2 determinant  */
	double temp2;                          /* second 2 by 2 determinant */
	double temp3;                          /* third 2 by 2 determinant  */
	double x;                              /* determinant               */
	int    count = 0;                      /* counts number of pos det  */
  
	rng.putSeed(0);

	for (i = 0; i < N; i++) {
	    for (j = 1; j <= 3; j++) {
		for (k = 1; k <= 3; k++) {
		    a[j][k] = rng.random();
		    if (j != k)
			a[j][k] = -a[j][k];
		}
	    }
	    temp1 = a[2][2] * a[3][3] - a[3][2] * a[2][3];
	    temp2 = a[2][1] * a[3][3] - a[3][1] * a[2][3];
	    temp3 = a[2][1] * a[3][2] - a[3][1] * a[2][2];
	    x = a[1][1] * temp1 - a[1][2] * temp2 + a[1][3] * temp3;
	    if (x > 0)
		count++;
	}

	//printf("\nbased on %ld replications ", N);
	System.out.print("\nbased on " + N + " replications ");  
	//printf("the estimated probability of a positive determinant is:\n");
	System.out.println("the estimated probability of a positive determinant is:\n");
	//printf("%11.9f", (double) count / N);
	System.out.print((double) count / N);
	//printf("\n");
	System.out.println();

    }
}
