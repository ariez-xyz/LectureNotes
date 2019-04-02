faker = require("faker");
fs = require('fs');
i = 0;

// depts
depts = [];
while(i < 100) {
    dept = faker.random.word();
    if(depts[dept] === undefined) {
        depts[dept] = faker.name.findName();
        i++;
    }
}

buffer = "";
deptNames = []
deptManagers = []

console.log("WARNING: PUTTING ALL DEPTS INTO DEPTS.TSV... REMOVE ALL BUT THE FIRST 10");
Object.entries(depts).forEach(entry => {
    buffer += entry[0] + "\t"; //dept
    deptNames.push(entry[0])
    buffer += entry[1] + "\t"; //manager
    deptManagers.push(entry[1])
    buffer += faker.address.city() + "\n"; // location
});

fs.writeFile('depts.tsv', buffer, (err) => {
  if (err) throw err;
  console.log('generated depts.tsv...');
});

// persons
persons = []
i = 0;
while(i < 100000) {
    name = faker.name.findName();
    if(persons[name] === undefined) 
        persons[name] = i++;
}

buffer = "";
personNames = []

Object.entries(persons).forEach(entry => { buffer += entry[1] + "\t"; //ssn
    buffer += entry[0] + "\t"; //name
    personNames.push(entry[0]);

    dept = Math.floor(Math.random() * 100)
    buffer += deptManagers[dept] + "\t"; //manager
    buffer += deptNames[dept] + "\t"; //dept

    buffer += faker.finance.amount() + "\t";
    buffer += faker.random.number() + "\n";
});

fs.writeFile('persons.tsv', buffer, (err) => {
  if (err) throw err;
  console.log('generated persons.tsv...');
});


// students
students = []
i = 0;
while(i < 100000) {
    if(i % 50 == 0)
        students[personNames[i]] = i++;
    else {
        name = faker.name.findName();
        if(students[name] === undefined) 
            students[name] = i++;
    }
}

// courses
i = 0;
courses = [];
while(i < 100) {
    course = faker.random.word();
    if(courses[i] === undefined) {
        courses[i] = course;
        i++;
    }
}

buffer = "";

Object.entries(students).forEach(entry => {
    buffer += entry[1] + "\t"; //ssn
    buffer += entry[0] + "\t"; //name
    course = Math.floor(Math.random() * 100)
    buffer += courses[course] + "\t";
    buffer += (1+Math.floor(faker.random.number() / 20000)) + "\n";
});

fs.writeFile('students.tsv', buffer, (err) => {
  if (err) throw err;
  console.log('generated students.tsv...');
});
