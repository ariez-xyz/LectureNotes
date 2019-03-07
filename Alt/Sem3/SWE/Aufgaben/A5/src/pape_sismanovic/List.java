package pape_sismanovic;

import java.util.ArrayList;

/**
 * Composite list object for Library tree strucutre implementation using composite design pattern
 */
public class List extends Component{
    private ArrayList<Component> children;

    /**
     * Instantiate new composite component
     * @param name Name of this component
     */
    public List(String name) {
        super(name);
        children = new ArrayList<>();
    }

    /**
     * Add a new component to this instances list of children
     * @param c Component to be added
     */
    @Override
    public void add(Component c) {
        children.add(c);
    }

    /**
     * Remove component from this instances list of children
     * @param c Component to be removed
     */
    @Override
    public void remove(Component c) {
        children.remove(c);
    }

    /**
     * @return Sum of prices of this instances children
     */
    @Override
    public int getPrice() {
        int sum = 0;

        for(Component c : children)
            sum += c.getPrice();

        return sum;
    }


    /**
     * Check if this instance has a child with name name
     * @param name Name of looked for component
     * @return Component with fitting name
     * @throws ItemNotFoundException If no suitable component is found among children
     */
    @Override
    public Component find(String name) throws ItemNotFoundException {
        if(this.getName().equals(name))
            return this;

        for(Component c : children)
            try {
                return c.find(name);
            } catch (ItemNotFoundException e) {
                continue;
            }

        throw new ItemNotFoundException("\"" + name + " not found in " + this.getName());
    }
}
