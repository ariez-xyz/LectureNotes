
/* -------------------------------------------------------------------------- 
 * A Monte Carlo simulation of a stochastic activity network.
 *
 * Name              : San.java
 * Author            : Jeff Mallozzi, Kerry Connell, Larry Leemis,Matt Duggan
 * Translation by    : Richard Dutton
 * Language          : Java
 * Latest Revision   : 6-16-06
 --------------------------------------------------------------------------- */

import java.io.*;
import java.text.*;
import java.lang.Math;

public class San{

    static final int MAXEDGE = 50;
    static final int MAXNODE = 50;
    static final int MAXPATHS = 50;
    static final long N = 10000;                 /* number of replications */

    long   M[][];
    long   Paths[][];
    double UpperLimit[];
    double p[];
    long   paths;
    long   nodes;
    long   edges;
    Rngs rngs;

    public San(){
	M = new long[MAXNODE][MAXEDGE];
	Paths = new long[MAXPATHS][MAXNODE];
	UpperLimit = new double[MAXEDGE];
	p = new double[MAXEDGE];

	rngs = new Rngs();
    }

    /* =========================== */
    double uniform(double a, double b)          /* use a < b */
    /* =========================== */
    {
	return (a + (b - a) * rngs.random());
    }

    /* ========================== */
    void getActivityDurations()
    /* ========================== */
    {
	int i;

	for (i = 1; i <= edges; i++)
	    p[i] = uniform(0.0, UpperLimit[i]);

	return;
    }

    /* ================ */
    void printPaths()
    /* ================ */
    {
	int i;
	int j;

	System.out.println();
	for (i = 1; i <= paths; i++) {
	    System.out.print("Path " + i + ": ");
	    j = 1;
	    while(Paths[i][j] != 0) {
		j++;
	    }
	    j--;
	    while(j > 0) {
		System.out.print("-" + Paths[i][j] + "-");
		j--;
	    }
	    System.out.println();
	}

	return;
    }

    /* =============================== */
    double timeToComplete(long node)
    /* =============================== */
    {
	int   k;
	long   l = 0;
	double tmax   = 0.0;
	double t  = 0.0;
  
	k = 1;
	while (l < M[(int)node][0]) {
	    if (M[(int)node][k] == -1) {
		t = timeToComplete(M[0][k]) + p[k];
		if (t >= tmax)
		    tmax = t;
		l++;
	    }
	    k++;
	}
	return(tmax);
    }

    /* ============================================= */
    long getPaths(long node, long step, long path)
    /* ============================================= */
    {
	int i = 1;
	int j;
	long found    = 0;
	long numpaths = 0;
	long total    = 0;
  
	while(found < M[(int)node][0]) {
	    if(M[(int)node][i] == -1) {
		numpaths = getPaths(M[0][i], step + 1, path + total);
		for (j = 0; j < numpaths; j++) {
		    Paths[(int)(path + j + total)][(int)step] = i;
		}
		total += numpaths;
		found++;
	    }
	    i++;
	}

	if(total == 0) {
	    Paths[(int)path][(int)step] = 0;
	    total = 1;
	}

	return(total);
    }

    /* ============================================= */
    void estimatePathProb()
    /* ============================================= */
    {
	int i;
	int j;
	int k;
	long   maxpath = -1;
	long   PathProb[] = new long[MAXPATHS]; 
	for(int c = 0; c<MAXPATHS; c++){
	    PathProb[c] = 0;    
	}
	double pathtime           = 0.0;
	double maxtime            = 0.0;
  
	for (i = 0; i < N; i++) {
	    getActivityDurations();

	    for (j = 1; j <= paths; j++) {
		k = 1;
		while(Paths[j][k] != 0) {
		    pathtime += p[(int)Paths[j][k]];
		    k++;
		}
		if(pathtime > maxtime) {
		    maxtime = pathtime;
		    maxpath = j;
		}
		pathtime = 0.0;
	    }
	    PathProb[(int)maxpath]++;
    
	    maxpath = 0;
	    maxtime = 0.0;
	}

	DecimalFormat f2 = new DecimalFormat("0.000000");
	System.out.print("\nCritical path probabilities:\n");
	for (i = 1; i <= paths; i++)
	    System.out.print(" -  " + i + "  - ");
	System.out.println();
	for (i = 1; i <= paths; i++)
	    System.out.print(" " + f2.format((double) PathProb[i]/N));
	System.out.println();

	return;
    }

    /* =================== */
    void defineNetwork()
    /* =================== */
    {
	int j;
	int k;

	edges = 9;
	nodes = 6;

	for (j = 0; j <= nodes; j++) {
	    for (k = 0; k <= edges; k++) {
		M[j][k] = 0;
	    }
	}
	M[1][1] = 1;
	M[2][1] = -1;
	M[1][2] = 1;
	M[3][2] = -1;
	M[1][3] = 1;
	M[4][3] = -1;
	M[2][4] = 1;
	M[3][4] = -1;
	M[2][5] = 1;
	M[5][5] = -1;
	M[3][6] = 1;
	M[4][6] = -1;
	M[3][7] = 1;
	M[6][7] = -1;
	M[4][8] = 1;
	M[6][8] = -1;
	M[5][9] = 1;
	M[6][9] = -1;

	for (j = 1; j <= nodes; j++) {
	    for (k = 1; k <= edges; k++) {
		if(M[j][k] == -1) {
		    M[j][0]++;
		}
		else if(M[j][k] == 1) {
		    M[0][k] = j;
		}
	    }
	}

	UpperLimit[1] = 3.0;
	UpperLimit[2] = 6.0;
	UpperLimit[3] = 13.0;
	UpperLimit[4] = 9.0;
	UpperLimit[5] = 3.0;
	UpperLimit[6] = 9.0;
	UpperLimit[7] = 7.0;
	UpperLimit[8] = 6.0;
	UpperLimit[9] = 3.0;

	paths = getPaths(nodes, 1, 1);

	System.out.print("Network read in:\n");
	System.out.print(edges + " edges.\n");
	System.out.print(nodes + " nodes.\n");
	System.out.print(paths + " paths.\n");

	return;
    }

    /* ============================ */
    void estimateCompletionTime()
    /* ============================ */
    {
	long   node;
	long   i;
	double time;
	double sumtime = 0.0;
  
	node = nodes; 

	for (i = 0; i < N; i++) {
	    getActivityDurations();
	    time = timeToComplete(node);
	    sumtime += time; 
	}

	//printf("\nFor %ld replications,", N);
	System.out.print("\nFor " + N + " replications,");
	//printf("\nthe estimated average time to complete the network is:\n");
	System.out.print("\nthe estimated average time to complete the network is:\n"); 
	DecimalFormat f2 = new DecimalFormat("##########0.00000");
	//printf("%11.5f\n", sumtime / (double) N);
	System.out.println(f2.format(sumtime / (double) N));

	return;
    }
  
    /* ============= */
    public static void main(String args[])
    /* ============= */
    {
	San san = new San();
	san.run();
    }

    void run(){
        defineNetwork();
        printPaths();
        rngs.putSeed(0);
        estimateCompletionTime();
        rngs.putSeed(0);
        estimatePathProb();
    }
}
