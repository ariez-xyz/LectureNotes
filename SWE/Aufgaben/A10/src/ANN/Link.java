package ANN;

/**
 * basically a preprocessor
 */
public class Link {
    private double[][] weights; // outputDimension x inputDimension
    private int inputDimension;
    private int outputDimension;

    /**
     * create new link with random weights.
     * @param inputDimension
     * @param outputDimension
     */
    public Link(int inputDimension, int outputDimension) {
        this.inputDimension = inputDimension;
        this.outputDimension = outputDimension;
        this.weights = new double[outputDimension][inputDimension];

        for (int i = 0; i < outputDimension; i++)
            for (int j = 0; j < inputDimension; j++)
                weights[i][j] = Math.random();
    }

    /**
     * creates "first" link
     * @param dimension
     */
    public Link(int dimension) {
        this.inputDimension = dimension;
        this.outputDimension = dimension;

        weights = new double[dimension][dimension];
        for (int i = 0; i < dimension; i++)
            weights[i][i] = 1d;
    }

    public double[] feed(double[] raw) {
        if (raw.length != inputDimension)
            throw new BadInputException(String.format("too much/little food for thought: expected array of length %d, got %d", inputDimension, raw.length));

        double[] prepared = new double[outputDimension];
        for (int i = 0; i < outputDimension; i++)
            prepared[i] = prepare(raw, weights[i]);
        return prepared;
    }

    /**
     * dot product of two vectors
     */
    private double prepare(double[] food, double[] weights) {
        double result = 0d;
        for (int i = 0; i < food.length; i++)
            result += food[i] * weights[i];
        return result;
    }

    /**
     * Set a specific weight
     * @param inputId equivalent ID of neuron in the previous layer
     */
    public void setWeight(int neuronId, int inputId, double newWeight) {
        weights[neuronId][inputId] = newWeight;
    }

    public double getWeight(int neuronID, int inputId) {
        return weights[neuronID][inputId];
    }
}
