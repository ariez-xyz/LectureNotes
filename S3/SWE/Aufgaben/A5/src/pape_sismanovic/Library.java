package pape_sismanovic;

import assignment5_int.Assignment5;
import org.xml.sax.SAXException;

import javax.xml.parsers.ParserConfigurationException;
import java.io.File;
import java.io.IOException;

/**
 * Points to root of a composite tree holding lists, books and CDs. Implements Assignment 5.
 */
public class Library implements Assignment5 {
    private Component root;

    /**
     * Read XML file holding Library data. Elements must have unique names, otherwise getPrice() becomes ambiguous
     * To preserve uniqueness of element names, sequential calls will override data of the last call.
     * @throws SAXException If file does not follow specified format
     * @throws IOException If file not found
     * @throws ParserConfigurationException Never, probably
     */
    @Override
    public void loadXml(File input) throws SAXException, IOException, ParserConfigurationException {
        root = new XMLFile(input).read();
    }

    /**
     * Returns price of the element with specified name, where price is defined as
     * The sum of the prices of all children        If called on a list
     * The price of the element                     If called on a book or CD
     * @param name Name of element
     * @return Price as defined above
     * @throws ItemNotFoundException If no item with specified name can be found
     */
    @Override
    public double getPrice(String name) throws ItemNotFoundException {
        return find(name).getPrice();
    }

    /**
     * @param name Name of component to be searched for
     * @return Component with specified name or null if not found.
     * @throws ItemNotFoundException If no item with specified name can be found
     */
    public Component find(String name) throws ItemNotFoundException {
        Component c = root.find(name);

        if (c == null)
            throw new ItemNotFoundException("\"" + name + "\" does not exist in the tree");

        return c;
    }
}
