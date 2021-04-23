package pape_sismanovic;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import java.io.File;
import java.io.IOException;

public class XMLFile {
    private File input;
    private final static int ELEMENT = 1;

    /**
     * Create new XMLFile instance "onto" a given file
     * @param input Input XML file
     */
    public XMLFile(File input) {
        this.input = input;
    }

    /**
     * Recursively read XML file passed at instantiation and create a corresponding tree structure of Component objects
     * @return Component object representing the root node of tree structure
     * @throws ParserConfigurationException Should never be thrown at all since we only generate default XML parsers
     * @throws IOException On file not found
     * @throws SAXException If input file does not follow the format specified in the assignment
     */
    public Component read() throws ParserConfigurationException, IOException, SAXException {
        DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
        DocumentBuilder db = dbf.newDocumentBuilder();
        Document doc = db.parse(input);
        return createComponentFromElement(doc.getDocumentElement());
    }

    private Component createComponentFromElement(Element root) throws SAXException {
        switch (root.getTagName()) {
            case "book":    return parseBook(root);
            case "cd":      return parseCD(root);
            case "list":    List list = new List(getAttribute(root, "name"));
                            NodeList children = root.getChildNodes();
                            for (int i = 0; i < children.getLength(); i++)
                                if (children.item(i).getNodeType() == ELEMENT)
                                    list.add(createComponentFromElement((Element) children.item(i)));
                            return list;
            default:        throw new SAXException("unsupported item: " + root.getTagName());
        }
    }

    private Book parseBook(Element book) throws SAXException {
        int isbn = getNumericalAttribute(book, "isbn");
        int price = getNumericalAttribute(book, "price");
        String name = getAttribute(book, "name");

        return new Book(name, price, isbn);
    }

    private CD parseCD(Element cd) throws SAXException {
        int price = getNumericalAttribute(cd, "price");
        String name = getAttribute(cd, "name");

        return new CD(name, price);
    }

    private String getAttribute(Element el, String name) throws SAXException {
        String attribute = el.getAttribute(name);
        if (attribute.equals(""))
            throw new SAXException("cannot find attribute " + name + " of " + el.getTagName());
        return attribute;
    }

    private int getNumericalAttribute(Element el, String name) throws SAXException {
        try {
            return Integer.parseInt(getAttribute(el, name));
        } catch (NumberFormatException e) {
            throw new SAXException("invalid value \"" + getAttribute(el, name) + "\" for " + name + " of " + el.getTagName());
        }
    }
}
