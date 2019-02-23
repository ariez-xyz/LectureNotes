#ifndef PERSONH
#define PERSONH

#include <string>
#include <vector>

using namespace std;

class Person {
    public:
        friend ostream& operator<<(ostream& out, const Person& p);
        friend bool operator<(const Person &a, const Person &b);

        Person(const string fName, const string lName, const string yearOfBirth);

        int getNumberOfRelatives() const {return directRelatives.size();}
        Person* getRelativeAt(int i) const {return directRelatives.at(i);}
        void putRelative(Person* p) {directRelatives.push_back(p);}

        string getFName() const;
        string getLName() const;
        string getYOB() const;

        int getDistance() const {return distance_;}
        void setDistance(int d) {distance_ = d;}

        int getVisited() const {return visited_;}
        void setVisited(int d) {visited_ = d;}

        string getId() const {return id_;}

    private:
        int distance_;
        int visited_;
        string fName_; 
        string lName_; 
        string yearOfBirth_; 
        string id_;
        vector<Person*> directRelatives;  
};
 
#endif
