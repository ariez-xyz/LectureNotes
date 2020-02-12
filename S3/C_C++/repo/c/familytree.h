#ifndef FAMTREE_H
#define FAMTREE_H
#define FAMTREE_SIZE 500
#include "person.h"

typedef struct {
    Person* persons[FAMTREE_SIZE]; // another hardcoded size limit
    int n;
} FamilyTree;

FamilyTree* newFamilyTree();
FamilyTree* familyTreeFromFile(char path[]);
void familyTreeToFile(FamilyTree* g);
Person* add(FamilyTree *g, Person *n);
Person* find(FamilyTree *g, Person *n);
int findNextSmallest(int from, FamilyTree* graph);
void sortFamilyTree(FamilyTree* g);
void deleteFamilyTree(FamilyTree* g);
#endif
