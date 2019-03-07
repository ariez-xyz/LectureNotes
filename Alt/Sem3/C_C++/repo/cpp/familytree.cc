#include "familytree.h"

#include <fstream>
#include <iostream>
#include <algorithm>

using namespace std;

FamilyTree::FamilyTree(string pathToFile) {
    persons_.reserve(findNumPersons(pathToFile)); // resizing vector breaks pointers...
    ifstream personenStream(pathToFile);
    
    string fName, lName, gender, YOB, YOD, fatherFName, fatherLName, fatherYOB, motherFName, motherLName, motherYOB;
    while(personenStream >> fName >> lName >> gender >> YOB >> YOD >> fatherFName >> fatherLName >> fatherYOB >> motherFName >> motherLName >> motherYOB) {

        int currentPos = insertIfNotPresent(fName, lName, YOB);
        int fatherPos = insertIfNotPresent(fatherFName, fatherLName, fatherYOB);
        int motherPos = insertIfNotPresent(motherFName, motherLName, motherYOB);

        if(fatherPos != -1) {
            persons_.at(currentPos).putRelative(&persons_.at(fatherPos));
            persons_.at(fatherPos).putRelative(&persons_.at(currentPos));
        }

        if(motherPos != -1) {
            persons_.at(currentPos).putRelative(&persons_.at(motherPos));
            persons_.at(motherPos).putRelative(&persons_.at(currentPos));
        }
    }
}

int FamilyTree::insertIfNotPresent(string fName, string lName, string YOB) {
    if(YOB == "0") // if unknown
        return -1;

    Person p(fName, lName, YOB);
    int pos;

    if(indexMap_.find(p.getId()) == indexMap_.end()) { // if not present
        pos = persons_.size();
        indexMap_[p.getId()] = pos;
        persons_.push_back(p);
    } else 
        pos = indexMap_[p.getId()];

    return pos;
}

int FamilyTree::indexOf(string id) {
    if(indexMap_.find(id) == indexMap_.end())
        return -1;
    else
        return indexMap_[id];
}

Person* FamilyTree::get(int index) {
    return &persons_.at(index);
}

int FamilyTree::findNumPersons(string pathToFile) {
    int persons = 0;
    string line;
    ifstream personenStream(pathToFile);
    while(getline(personenStream, line))
        persons++;
    return persons;
}

void FamilyTree::sort(){
    std::sort(persons_.begin(), persons_.end(), [](const Person& a, const Person& b) {
            if(a.getDistance() == b.getDistance()) {
                if(a.getYOB() == b.getYOB()) {
                    if(a.getLName() == b.getLName()) {
                        return a.getFName() < b.getFName();
                    } else
                        return a.getLName() < b.getLName();
                } else
                    return a.getYOB() < b.getYOB();
            } else 
                return a.getDistance() < b.getDistance();
            });
}

int FamilyTree::size() {
    return persons_.size();
}
