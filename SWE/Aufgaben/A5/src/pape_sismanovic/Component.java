package pape_sismanovic;

/**
 * Abstract base class for Library implementation using Composite design pattern
 */
public abstract class Component {
    private String name;

    /**
     * Create new component with given (immutable) name
     * @param name Name of component
     */
    public Component(String name) {
        this.name = name;
    }

    /**
     * @return Name of this component
     */
    public String getName() {
        return name;
    }

    /**
     * Attempt to add a new component to this instance
     * @param c Component to be added
     */
    public void add(Component c) {
        throw new UnsupportedOperationException();
    }

    /**
     * Attempt to remove a component from the children of this instance
     * @param c Component to be removed
     */
    public void remove(Component c) {
        throw new UnsupportedOperationException();
    }

    /**
     * Check if this components name is equal to parameter
     * @param name Name of looked for component
     * @return this if this.name is equal to name
     * @throws ItemNotFoundException If this.name is not equal to name
     */
    public Component find(String name) throws ItemNotFoundException {
        if(this.name.equals(name))
            return this;

        throw new ItemNotFoundException("\"" + name + " not found in " + this.getName());
    }

    /**
     * Price is defined as sum of prices of kids for a composite object
     * @return Price of this item
     */
    public abstract int getPrice();
}
