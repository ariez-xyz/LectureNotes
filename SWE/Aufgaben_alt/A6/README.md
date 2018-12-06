# A6_WS2018

Assignment 6: Decorator
--------------------------------

Study the decorator pattern [1].

Get an overview of the ``java.io`` package [2].

Scan the class-javadocs of java.io classes. Which ones are suitable for your needs?
Can you spot other implementations of the decorator pattern in this package?

Write two decorator classes: One that decorates an existing Java class for 
(i) reading and one for (ii) writing character streams.

Use the following class names:
(i) ``MorseReader``  
(ii) ``ROT13Writer``  

Make sure that each one of your two decorator classes decorates an existing Java class from the ``java.io`` package. Which class should you decorate, such that you really benefit from the pattern.

Note: the assignment is about CHARACTER streams (see above).

The reading decorator shall implement a morse code translator [3,4]. 
For example, a file containing ``.... . .-.. .-.. ---`` will be read as ``hello``.

The writing decorator shall implement the ROT13 cipher algorithm.



Be prepared for questions: UML, pros/cons, requirements, ...

[1] Design Patterns. Elements of Reusable Object-Oriented Software; Gamma et al.  
[2] https://docs.oracle.com/javase/8/docs/api/java/io/package-summary.html  
[3] https://en.wikipedia.org/wiki/Morse_code  
[4] https://raw.githubusercontent.com/jvcleave/example-ofFile-MorseCode/master/bin/data/morse.csv  
