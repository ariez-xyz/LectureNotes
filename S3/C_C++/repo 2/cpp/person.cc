#include "person.h"

#include <string>
#include <iostream>

Person::Person(const string pfName, const string plName, const string pyearOfBirth) 
    :   fName_ (pfName), lName_ (plName), yearOfBirth_ (pyearOfBirth), distance_ (-1), visited_ (0), id_ (pfName + plName + pyearOfBirth)
{}

string Person::getFName() const {
    return fName_;
}

string Person::getLName() const {
    return lName_;
}

string Person::getYOB() const {
    return yearOfBirth_;
}

ostream& operator<<(ostream& out, const Person& p) {
    out << to_string(p.distance_) + " " + p.yearOfBirth_ + " " + p.lName_ + " " + p.fName_;
    return (out);
}

