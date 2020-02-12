#include <stdlib.h>
#include "personqueue.h"

PersonQueue* newPersonQueue() {
    PersonQueue* q = (PersonQueue* ) malloc (sizeof (PersonQueue));
    q->qbegin = 0;
    q->qend = 0;
    return q;
}

void enqueue(PersonQueue* q, Person* n) {
    q->persons[q->qend] = n;
    q->qend++;
}

Person* dequeue(PersonQueue* q) {
    q->qbegin++;
    return q->persons[q->qbegin - 1];
}

int isEmpty(PersonQueue* q) {
    return 1 - (q->qbegin < q->qend);
}
