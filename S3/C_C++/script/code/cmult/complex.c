#include "complex.h"

void complexMult(double rx, double ix, double ry, double iy, double *rz, double *iz)
{
    *rz = rx * ry - ix * iy;
    *iz = rx * iy + ix * ry;
}
