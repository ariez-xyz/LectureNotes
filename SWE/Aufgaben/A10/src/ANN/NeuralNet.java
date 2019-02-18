package ANN;

import Functions.ActivationFunction;

/**
 * Represents a simple artificial neural network
 */
public class NeuralNet {
    private Link[] links;
    private Layer[] layers;
    private int n;

    /**
     * Create new NeuralNet with specified layer sizes and functions
     * @param layerSizes Integer array specifying number of neurons in ech layer
     * @param fs ActivationFunction instance at position i specifies activation function of layer i
     */
    public NeuralNet(int[] layerSizes, ActivationFunction[] fs) {
        this.n = layerSizes.length;
        links = new Link[n];
        layers = new Layer[n];

        // special link for first layer
        int previousOutputDimension = layerSizes[0];
        links[0] = new Link(layerSizes[0]);
        layers[0] = new Layer(layerSizes[0], fs[0]);

        // link rest of layers together
        for (int i = 1; i < n; i++) {
            links[i] = new Link(previousOutputDimension, layerSizes[i]);
            layers[i] = new Layer(layerSizes[i], fs[i]);
            previousOutputDimension = layerSizes[i];
        }
    }

    /**
     * Process some input
     * @return Result of computations
     */
    public double[] think(double[] input) {
        double[] thought = new double[input.length];
        for (int i = 0; i < n; i++)
            thought = layers[i].think(links[i].feed(thought));
        return thought;
    }

    /**
     * Set activation function of a specific neuron
     */
    public void setActivationFunction(int layerId, int neuronId, ActivationFunction f) {
        // TODO implement error handling here
        layers[layerId].setActivationFunction(neuronId, f);
    }

    /**
     * Set activation function of a layer
     */
    public void setActivationFunction(int layerId, ActivationFunction f) {
        // TODO implement error handling here (out of bounds)
        layers[layerId].setActivationFunction(f);
    }

    /**
     * Set weight of a single input of a single neuron
     */
    public void setWeight(int linkId, int neuronId, int inputId, double newWeight) {
        if (linkId == 0) {
            // do not change weight of input
            System.err.println("Warning: ignored attempt to set input weight");
            return;
        }
        // TODO implement error handling here and in subsequent calls...
        links[linkId].setWeight(neuronId, inputId, newWeight);
    }

    /**
     * @return Layer object at specified index
     */
    public Layer getLayer(int index) {
        return layers[index];
    }

    /**
     * @return Number of layers
     */
    public int getN() {
        return n;
    }

    /**
     * @return Link object at specified index
     */
    public Link getLink(int index) {
        return links[index];
    }
}
