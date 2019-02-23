#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include "familytree.h"
#include "person.h"

FamilyTree* newFamilyTree()
{
    FamilyTree* g = (FamilyTree* ) malloc (sizeof (FamilyTree));
    g->n = 0;
    return g;
}

// add person to graph or return equivalent previously added person
Person* add(FamilyTree* g, Person* n) {
    int i;
    for(i = 0; i < g->n; i++) 
        if(comparePersons(n, g->persons[i]))
            return g->persons[i];

    if(g->n >= FAMTREE_SIZE) {
        fprintf (stderr, "ERROR: Family tree overflow. Input file size cannot exceed %d persons\n", FAMTREE_SIZE);
        exit(1);
    }

    g->persons[g->n] = n;
    g->n++;
    return n;
}

Person* find(FamilyTree* g, Person* n) {
    int i;
    for(i = 0; i < g->n; i++) 
        if(comparePersons(n, g->persons[i]))
            return g->persons[i];
    return NULL;
}

int findNextSmallest(int from, FamilyTree* g) {
    int min = from;
    int i;
    for(i = from + 1; i < g->n; i++) {
        Person* current = g->persons[i];
        if(current->distance < g->persons[min]->distance)
            min = i;
        else if(current->distance == g->persons[min]->distance) {
            if (current->yob < g->persons[min]->yob)
                min = i;
            else if(current->yob == g->persons[min]->yob)
                if(strcmp(current->fname, g->persons[min]->fname) < 0 
                ||(strcmp(current->fname, g->persons[min]->fname) == 0 && strcmp(current->lname, g->persons[min]->lname) < 0)) 
                    min = i;
        }
    }
    return min;
}

FamilyTree* familyTreeFromFile(char path[]) {
    FILE* F = fopen(path, "r");
    if (F == NULL) {
        fprintf (stderr, "ERROR: Could not open file %s\n", path);
        exit(1);
    } 

    FamilyTree* g = newFamilyTree();

    int r;
    while(!feof(F)) {
        Person* newChild = newPerson();
        Person* newParent1 = newPerson();
        Person* newParent2 = newPerson();

        r = fscanf(F, "%99s %99s %*1c %d %*d \
                       %99s %99s %d \
                       %99s %99s %d", \
                       newChild->fname, newChild->lname, &newChild->yob, \
                       newParent1->fname, newParent1->lname, &newParent1->yob, \
                       newParent2->fname, newParent2->lname, &newParent2->yob);

        if(isSensiblePerson(newChild)) {
            Person* child = add(g, newChild);
            if(isSensiblePerson(newParent1)) {
                Person* parent1 = add(g, newParent1);
                addNeighbor(child, parent1);
            }
            if(isSensiblePerson(newParent2)) {
                Person* parent2 = add(g, newParent2);
                addNeighbor(child, parent2);
            }
        }
    }

    fclose(F);
    return g; 
}

void familyTreeToFile(FamilyTree* g) {
    FILE* verwandte = fopen("verwandte.dat", "w");
    int i;
    for(i = 0; i < g->n; i++) 
        fprintf(verwandte, "%d %s %s %d\n", g->persons[i]->distance, g->persons[i]->fname, g->persons[i]->lname, g->persons[i]->yob);
    fclose(verwandte);
}

void sortFamilyTree(FamilyTree* g) {
    int i;
    for(i = 0; i < g->n; i++) {
        int min = findNextSmallest(i, g);
        Person* help = g->persons[i];
        g->persons[i] = g->persons[min];
        g->persons[min] = help;

    }
}

void deleteFamilyTree(FamilyTree* g) {
    int i;
    for(i = 0; i < g->n; i++)
        free(g->persons[i]);
    free(g);
}
