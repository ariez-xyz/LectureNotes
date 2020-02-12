#include <stdio.h>
#include <stdlib.h>
#include "complex.h"

int main(int argc, char *argv[]) {
    double rz, iz;
    if(argc != 5)
        exit(1);
    //atof = string to float conversion
    complexMult(atof (argv[1]), atof (argv[2]), atof (argv[3]), atof (argv[4]), &rz, &iz);
    printf ("%f %f\n", rz, iz);
}

