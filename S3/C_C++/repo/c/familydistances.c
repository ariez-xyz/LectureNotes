#include <stdlib.h>
#include "familydistances.h"

void visitNeighbors(Person* n, PersonQueue* q) { // simple BFS variant
    int i;
    for(i = 0; i < n->neighborsCount; i++)
        if(!n->neighbors[i]->visited) {
            n->neighbors[i]->distance = n->distance + 1;
            n->neighbors[i]->visited = 1;
            enqueue(q, n->neighbors[i]);
        }
    n->visited = 2;
}

void calcPersonDistances(Person* source) {
    source->distance = 0;
    PersonQueue* q = newPersonQueue();
    enqueue(q, source);
    while(!isEmpty(q))
        visitNeighbors(dequeue(q), q);
    free(q);
}
