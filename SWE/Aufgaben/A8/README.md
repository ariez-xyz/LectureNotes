# A8_WS2018
Unit-Testing

Assignment 8: Unit testing, code coverage
----------------------------------------------

**JUnit and some background.**

1.  What is test driven development (TDD), unit testing?
    * TDD: 
        * Writing tests for the software before writing the software. 
        * 3 phases: *Red/Green/Refactor*:
            * first, write tests that make the software fail (red light)
            * make the software pass the tests and nothing else! (green light)
            * finally, refactor; most importantly: **remove code duplication**. This must be done before moving to the next test
        * Rules:
            * Write only enough of a unit test to fail.
            * Write only enough production code to make the failing unit test pass.
        * Advantages: easy debugging - only the latest changes. faster in the long run
    * Unit testing:
        * 4 phases of testing: *unit, integration, system, acceptance*
        * Unit testing tests separate units/components of the codebase
        * Integration tests test subsystems (interactions between units)
        * System tests test the whole system
        * Acceptance tests is basically beta
1.  Get familiar with JUnit 5 (www.junit.org); (use version 5, which is based on Java annotations).
    * Utilizes test classes with annotated test methods
    * 
1.  Test your stack (from Assignment 2).
	* Extend your stack implementation by a getSize() and a peek() method, also make sure to throw appropriate exceptions when you perform a pop() on an empty stack, for example.
   	* Write JUnit tests to test all your stacks. Use the special features for testing for expected exceptions in JUnit. 
    * Note: You have two stack classes, but you must not duplicate your test code in order to test both stacks!!
1.	Test the calculator (from Assignment 2).
    * Write several tests for the RPN calculator.
    * Write also tests to check the behavior on invalid input.

**Code coverage analysis.**

1. Try to have your whole code of Assignment 2 covered by unit tests.
1. Find and try a code coverage analysis tool (e.g. Eclipse plug-in).
1. Analyze the coverage of your code and improve your tests if necessary.
1. Make a screenshot of the coverage analysis tool that documents your efforts and submit the image (jpeg).

**Required submissions (git):**

* the .java file(s) for the stacks and the calculator:
```
 src/assignment2_int/Calculator.java  
    /stud1_stud2/YourStackClass1.java, ...
```
* the .java file(s) for the junit-tests (note: not in `src`, but in `test`):
```
test/stud1_stud2/YourTestClass1.java, ...
```
* the screenhot of the coverage analysis:
```
stud1_stud2.jpg
```
