#include <fstream>
#include <iostream>

#include "distances.h"
#include "person.h"
#include "familytree.h"

using namespace std;

void usage() {
    cout << "usage: ./programm <personen.dat> <stand.dat> <vorname> <nachname> <geburtsjahr>" << endl;
    exit(1);
}

// Passt.  Rade
int main(int argc, char* argv[]) {
    if(argc != 6)
        usage();

    string personenFile(argv[1]);
    string standFile(argv[2]);
    string source_fname(argv[3]);
    string source_lname(argv[4]);
    string source_yob(argv[5]);
    string sourceId = source_fname + source_lname + source_yob;
    
    FamilyTree tree(personenFile);

    calcPersonDistances(tree.get(tree.indexOf(sourceId)));

    tree.sort();

    ofstream out("verwandte.dat");
    for(int i = 0; i < tree.size(); i++)
        out << *tree.get(i) << endl;
}
