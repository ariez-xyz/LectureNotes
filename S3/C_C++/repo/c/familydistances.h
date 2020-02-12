#ifndef FAMDIST_H
#define FAMDIST_H
#include "personqueue.h"

void visitNeighbors(Person *n, PersonQueue *q);
void calcPersonDistances(Person* source);
#endif
