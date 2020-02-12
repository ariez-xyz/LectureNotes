#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "person.h"
#include "familytree.h"
#include "familydistances.h"
#include "personqueue.h"

int main(int argc, char* argv[]) {
    if(argc != 6) {
        printf("usage:\n");
        printf("programm personen.dat stand.dat vorname nachname geburtsjahr\n");
        exit(1);
    }
    printf ("Attempting to parse file '%s'... ", argv[1]);
    FamilyTree* g = familyTreeFromFile(argv[1]);
    puts("done.");

    Person* sourceHelper = newPerson();
    strcpy(sourceHelper->fname, argv[3]);
    strcpy(sourceHelper->lname, argv[4]);
    sourceHelper->yob = atoi(argv[5]);

    Person* source = find(g, sourceHelper);
    free(sourceHelper);

    if(source == NULL) {
        printf("ERROR: Could not find '%s %s %s' in input. Exiting...\n", argv[3], argv[4], argv[5]);
        exit(1);
    } else {
        printf("Found person '%s %s %d'.\nStarting BFS...", source->fname, source->lname, source->yob);
    }

    calcPersonDistances(source);
    puts("done.");

    printf("sorting and writing to verwandte.dat... ");
    sortFamilyTree(g);    

    familyTreeToFile(g);
    puts("done");

    deleteFamilyTree(g);
}

