#ifndef FAMTREEH
#define FAMTREEH

#include <string>
#include <vector>
#include <unordered_map>

#include "person.h"

class FamilyTree {
    public:
        FamilyTree(string pathToFile);
        Person* get(int index);
        int indexOf(string id);
        void sort();
        int size();
    private:
        int insertIfNotPresent(string fname, string lname, string yob);
        int findNumPersons(string pathToFile);
        vector<Person> persons_;
        unordered_map<string, int> indexMap_;
};
#endif
