#ifndef PERSON_H
#define PERSON_H
#define MAX_NEIGHBORS_COUNT 500

// represents a person in a graph, also holds additional data about the corresponding person
typedef struct PersonStructure {
    char fname[100]; // year of birth + last name + first name [concatenated]
    char lname[100];
    int yob;
    int visited;
    int distance;
    struct PersonStructure* neighbors[MAX_NEIGHBORS_COUNT];
    int neighborsCount;
} Person;

Person* newPerson();
int comparePersons(Person *n1, Person *n2);
void addNeighbor(Person *n1, Person *n2);
int isSensiblePerson(Person *n);
void pn(Person* n);
#endif
