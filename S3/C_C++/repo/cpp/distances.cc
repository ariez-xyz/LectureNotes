#include "distances.h"

#include <iostream>

using namespace std;

void calcPersonDistances(Person* source) {
    vector<Person*> bfsqueue; // queue does not appear to work for some reason
    int qstart = 0;

    source->setDistance(0);
    bfsqueue.push_back(source);

    while(qstart < bfsqueue.size()) {
        Person* current = bfsqueue.at(qstart);
        //cout << *current << endl;
        for(int i = 0; i < current->getNumberOfRelatives(); i++) {
            Person* relative = current->getRelativeAt(i);
            if(relative->getVisited() == 0) {
                relative->setDistance(current->getDistance() + 1);
                relative->setVisited(1);
                bfsqueue.push_back(current->getRelativeAt(i));
            }
        }
        current->setVisited(2);
        qstart++;
    }
}
