#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "person.h"

void pn(Person* n) { //convenience method for printing persons
    printf("%d %s %s %d\n", n->distance, n->fname, n->lname, n->yob);
}

Person* newPerson() {
    Person* n = (Person*) malloc (sizeof (Person));
    n->neighborsCount = 0;
    n->distance = -1;
    n->visited = 0;
    return n;
}

int comparePersons(Person* n1, Person* n2) {
    if(strcmp(n1->fname, n2->fname) != 0
    || strcmp(n1->lname, n2->lname) != 0
    || n1->yob != n2-> yob)
        return 0;
    return 1;
}

void addNeighbor(Person* n1, Person* n2) {
    if(n1->neighborsCount >= MAX_NEIGHBORS_COUNT || n2->neighborsCount >= MAX_NEIGHBORS_COUNT) {
        fprintf (stderr, "ERROR: Neighbor array overflow. A person cannot have more than %d direct relatives\n", MAX_NEIGHBORS_COUNT);
        exit(1);
    }
    
    n1->neighbors[n1->neighborsCount] = n2;
    n1->neighborsCount++;
    n2->neighbors[n2->neighborsCount] = n1;
    n2->neighborsCount++;
}

// check if person represents unknown parent, i.e. is not "- - 0"
int isSensiblePerson(Person* n) {
    if(n->yob == 0)
        return 0;
    return 1;
}
