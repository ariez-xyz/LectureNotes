# A10_WS2018
ANN

Assignment 10: ANN XML/API
---------------------------

Understand the differences between API, Library, and Framework. (Are there any?)
Take a look at the Facade design pattern [GOF].

Design an Application Programming Interface (API) to create and configure an artificial neural network (ANN).
Write a Java Library implementing this API. 

You can make it simple, i.e., stick to the assumptions below. However, spend a few minutes to think about what it would mean to make the system more flexible (for example, the layers are potentially not fully connected).

Assumptions and requirements: 
1. The ANN is layered (configurable number of layers)
2. Each layer has a configurable number of neurons
3. Layers are fully connected
4. There is one activation function per layer
5. Optional: A neuron may have a separate activation function (this 'overrules' the activation function of the layer)
6. The initial values should be (somehow?!) configurable. First, think about why. Then think about what makes sense and how to realize this in Java (you can/should support multiple ways of configuration).

Design an XML-format to represent an ANN configuration and write a sample XML file.

Write an XML parser that reads in your configuration and that uses your API to create the network.
