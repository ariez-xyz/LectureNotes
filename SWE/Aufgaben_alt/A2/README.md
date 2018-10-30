# A2_WS2018

----------------------------------------
Assignment 2: Stack-based RPN Calculator
----------------------------------------

Implement an interface for a stack (LIFO).

Write two different stack implementations:
One that is based on an array with constant size; the other shall be based on a linked list. 
Both classes shall implement the interface above.
Use Generics.

The stack classes must not rely on the Java Collections Framework. 
Implement them yourself. Go for a simple solution, but include some basic errorhandling
 (e.g. pop-operation on an empty stack).
 
Use your stacks to implement an RPN (reverse-polish-notation) calculator.
The calculator shall work with both types of stacks (and any other implementation of the stack interface) 
without any modification (see "dependency injection").
Your calculator class must have a default constructor and must implement the following interface:

```java
package assignment2_int;

public interface Calculator {
   /** returns the result or throws an (Runtime-) Exception if anything goes wrong (e.g. illegal input) */
   public double calc(String[] input);
}
``` 

The program takes as input an expression in RPN and outputs the calculated result. 
The program shall support the 4 basic arithmetical operations `+ - * /`. 

```
Example:
input: {"1.5", "2", "+", "3", "4", "-", "*"}
output: -3.5
```

Note: The character * has a special meaning when passed as a command line argument. 
Double quoting works for some systems (not for all): E.g. > `java Calc 1.5 2 + 3 4 - "*"`

Work in teams of two!
See the PS homepage for Team/Packaging instructions!
