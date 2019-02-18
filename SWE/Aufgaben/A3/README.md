# A3_WS2018

------------------------------
Assignment 3: Strategy pattern
------------------------------

Write a program that takes as input a pair of strings (s, t) and outputs one of the following distances (differences) between s and t:

a) the difference between the length of s and the length of t

b) the Levenshtein distance (you may use an existing implementation, but make a comment in your code (e.g. cite the author))

c) the Hamming distance (for the Hamming distance a certain property of s and t must hold; think about what to do when the property does not hold.)

Use the Strategy-Pattern [1] to implement the different algorithms! The Strategy interface is given (`src/assignment3_int/StringComparer.java`):

```java
package assignment3_int;

public interface StringComparer {
	
   /** returns the distance between two strings, or throws some RuntimeException if anything goes wrong */
   public int compare(String s, String t);
}
```

Your programm shall implement the following interface (`src/assignment3_int/Assignment3.java`):
```java
package assignment3_int;

public interface Assignment3 {
	
   /** returns the distance between two strings, or throws some RuntimeException if anything goes wrong */
   public int getDistance(String s, String t);
   
   /** sets the strategy for calculating the distance between two strings */
   public int setStringComparison(StringComparer c);
}
```

Note: The class implementing the interface `Assignment3` MUST NOT know about the three existing algorithms.
Provide a separate class containing the main method, where the user can decide on the kind of distance that should be calculated.

Be prepared to
- explain the Strategy-Pattern in detail
- explain how the Levenshtein algorithm works
- draw the UML diagram (note: the notation in [1] is not UML)
- explain your implementation

Work in teams of two!

[1] Design Patterns. Elements of Reusable Object-Oriented Software; Gamma et al.
