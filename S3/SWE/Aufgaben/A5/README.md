# A5_WS2018
Composite Design Pattern

Write a program that can manage a list of items (e.g. books, cds, ...). An item may again be a list of items, etc.
The program shall read an XML-file/string and build the required data structure. 
Use the Composite design pattern [1] to represent items and lists.
The XML data is given below. Each item has a price. 
The price of a list is considered to be the sum of all its items (note that such an item may be a list again)!

The program shall take the name of an item as input from the user and output its price.
```
E.g. input="L2" output="30"
```

Note: You may want to take a look at DOM (`javax.xml.parsers.*` and `org.w3c.dom.*` which are part of the JDK) 
for reading the XML file [2]. There are other ways as well (StAX, SAX). 
However, do not use any classes (e.g. for XML processing) that are not part of the JDK!!

This is an example input file: (see input.xml)
```
<list name="root">
 <book name="B1" price="30" isbn="123"/>
 <list name="L1">
  <book name="B2" price="20" isbn="234"/>
  <list name="L2">
   <cd name="C1" price="15"/>
   <cd name="C2" price="5"/>
   <book name="B3" price="10" isbn="345"/>
  </list>
  <cd name="C3" price="15"/>
  <book name="B4" price="60" isbn="456"/> 
 </list>
</list>
```

Your programm shall implement the following interface (`src/assignment5_int/Assignment5.java`):

```java
package assignment5_int;

public interface Assignment5 {
   /** loads the xml file or throws an Exception if anything goes wrong */
   public void loadXml(File input) throws Exception;
   
   /** returns the price of an item (cd, book, or list) */
   public double getPrice(String item);
}
```

What shall we do if the `item` in `getPrice` does not exist? Find a good way to handle this!

Design note: XML should be handled at one single place in your implementation. It is not a good design to spread XML specific code in your model class(es). Why?

Be prepared to answer questions about the Composite design pattern in the lecture (advantages, disadvantages, UML diagram, ...)!!!


This might help you reading the file:
```java
//some basic code to read an xml file. 
DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
DocumentBuilder db = dbf.newDocumentBuilder();
Document doc = db.parse(new File(fileName));
Element root = doc.getDocumentElement();

//some dummy code...... TODO: find a good way to access the elements and their attributes (helper methods)
NodeList nl = root.getChildNodes();
for (int i=0;i<nl.getLength();i++){
  Node n = nl.item(i);
  int type = n.getNodeType();
  if (type == Node.ELEMENT_NODE){
    Element e = (Element) n;
    System.out.println(e.getTagName() + ": " + e.getAttribute("name"));
  }
}
```


[1] Design Patterns. Elements of Reusable Object-Oriented Software; Gamma et al.

[2] https://docs.oracle.com/javase/tutorial/jaxp/dom/readingXML.html