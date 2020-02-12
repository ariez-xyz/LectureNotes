#ifndef PERSQ_H
#define PERSQ_H
#define PERSQ_SIZE 500
#include "person.h"

typedef struct {
    Person* persons[PERSQ_SIZE];
    int qbegin; // starting index of queue (inclusive)
    int qend; // end index of queue (exclusive)
} PersonQueue;

PersonQueue* newPersonQueue();
Person* dequeue(PersonQueue *q);
void enqueue(PersonQueue *q, Person *n);
int isEmpty(PersonQueue *q);
#endif
