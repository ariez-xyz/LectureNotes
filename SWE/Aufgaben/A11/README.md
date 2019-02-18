# A11_WS2018

Assignment 11: Digit Recognition (machine learning)
--------------------------------------------------

The assignment is about porting (and reengineering) a program for classifying hand-written digits using a machine learning approach. The original program is available at [1] (as a python notebook `part2_neural_network_mnist_data`). It works with the MNIST database [2] containing 60,000 samples. A CSV-representation of the data is available at [3]. 

1)
Get familiar with the python program for recognizing hand-written digits.
To test it, adjust the paths to the csv-files.
Be sure to understand the theoretical background (ANN, backpropagation, ...) as well as the python code.

2)
Port this functionality to Java. Your program must not require any library that is not part of the JDK.

As part of your reengineering effort, make sure that your approach follows a well-structured OO design.
Avoid code duplication wherever possible.

Implement the following interface and make sure that the implementing class works with the default constructor:

```java
package assignment11_int;

import java.io.File;

/** Interface for assignment 1 */
public interface Assignment11 {
   
   /** loads the .csv file with the training data or throws an Exception if anything goes wrong; returns true iff the initialization completed successfully. */
   public boolean init(File csvTrainingData) throws Exception;
   
   /** trains the net; returns true iff the training phase completed successfully. */
   public boolean train() throws Exception;
   
   /** tries to recognize the digit represented by csvString; returns the digit */
   public int recognize(String csvString) throws Exception;
}
```

You may use the `Matrix` class in the package `assignment11_int`.

The format of the csvString in the `recognize` method is the same as for the training data (without the label). Thus it is a string of 28x28=784 values separated by commas and it represents a single digit.

Note: Since your program will be based on someone else's code, give credit where credit is due (mention the original author and put the link to his site in your javadoc).

[1] https://github.com/makeyourownneuralnetwork/makeyourownneuralnetwork 

[2] http://yann.lecun.com/exdb/mnist/ 

[3] https://pjreddie.com/projects/mnist-in-csv/ 
