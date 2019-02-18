import ANN.NeuralNet;
import Functions.ActivationFunction;
import Functions.FunctionRepository;
import Parser.ANNParser;

import java.util.Arrays;

/**
 * API for NeuralNetwork class
 */
public class ANN {
    NeuralNet net;

    /**
     * Create ANN from XML
     */
    public ANN(String pathToXml) throws Exception {
        this.net = new ANNParser().parse(pathToXml);
    }

    /**
     * Create ANN with specified layer sizes and default activation function
     */
    public ANN(int... layerSizes) {
        this(FunctionRepository.DEFAULT, layerSizes);
    }

    /**
     * Create ANN with specified layer sizes and activation function
     */
    public ANN(String activationfunction, int... layerSizes) {
        ActivationFunction[] fns = new ActivationFunction[layerSizes.length];
        Arrays.fill(fns, FunctionRepository.get(activationfunction));
        this.net = new NeuralNet(layerSizes, fns);
    }

    /**
     * Create ANN with specified layer sizes and activation functions
     */
    public ANN(ActivationFunction[] fs, int... sizes) {
        this.net = new NeuralNet(sizes, fs);
    }

    /**
     * Process some input and return the results
     */
    public double[] think(double[] input) {
        return net.think(input);
    }

    /**
     * Set all input weights of a specific neuron
     */
    public void setInWeights(int layerId, int neuronId, double[] newWeights) {
        if(layerId <= 0)
            throw new ANNException("please provide a layer id greater than 0");
        if(neuronId < 0)
            throw new ANNException("please provide a non negative neuron id");
        if(layerId > net.getN())
            throw new ANNException(String.format("cannot set input weights for layer %d: net only has %d layers", layerId, net.getN()));
        if(neuronId > net.getLayer(layerId).getSize())
            throw new ANNException(String.format("cannot set input weights for neuron %d: layer %d only has %d neurons", neuronId, layerId, net.getLayer(layerId).getSize()));
        if(newWeights.length != net.getLayer(layerId).getSize())
            throw new ANNException("please provide a suitable number of weights");

        for (int i = 0; i < net.getLayer(layerId).getSize(); i++)
            net.setWeight(layerId, neuronId, i, newWeights[i]);
    }

    /**
     * Set all output weights of a specific neuron
     */
    public void setOutWeights(int layerId, int neuronId, double[] newWeights) {
        if(layerId < 0)
            throw new ANNException("please provide a non negative layer id");
        if(neuronId < 0)
            throw new ANNException("please provide a non negative neuron id");
        if(layerId >= net.getN())
            throw new ANNException(String.format("cannot set output weights for layer %d: net only has %d layers", layerId, net.getN()));
        if(neuronId > net.getLayer(layerId).getSize())
            throw new ANNException(String.format("cannot set output weights for neuron %d: layer %d only has %d neurons", neuronId, layerId, net.getLayer(layerId).getSize()));
        if(newWeights.length != net.getLayer(layerId + 1).getSize())
            throw new ANNException("please provide a suitable number of weights");

        for (int i = 0; i < net.getLayer(layerId + 1).getSize(); i++)
            net.setWeight(layerId + 1, i, neuronId, newWeights[i]);
    }

    /**
     * Set a specific weight
     */
    public void setSpecificWeight(int layerId, int neuronId, int inputId, double newWeight) {
        if(neuronId < 0)
            throw new ANNException("please provide a non negative neuron id");

        if(layerId > net.getN())
            throw new ANNException(String.format("cannot set input weight for layer %d: net only has %d layers", layerId, net.getN()));

        if(neuronId > net.getLayer(layerId).getSize())
            throw new ANNException(String.format("cannot set input weight for neuron %d: layer %d only has %d neurons", neuronId, layerId, net.getLayer(layerId).getSize()));

        net.setWeight(layerId, neuronId, inputId, newWeight);
    }

    /**
     * Set activation function of some layer
     */
    public void setLayerFunction(int layerId, String activationfunction) {
        net.setActivationFunction(layerId, FunctionRepository.get(activationfunction));
    }

    /**
     * Set activation function of some neuron
     */
    public void setNeuronFunction(int layerId, int neuronId, String activationfunction) {
        net.setActivationFunction(layerId, neuronId, FunctionRepository.get(activationfunction));
    }

    /**
     * Get input weights of some neuron
     */
    public double[] getInWeights(int layerId, int neuronId) {
        if(layerId <= 0 || layerId > net.getN())
            throw new ANNException("please provide a suitable layer id");
        if(neuronId < 0 || neuronId > net.getLayer(layerId).getSize())
            throw new ANNException("please provide a suitable neuron id");

        double[] wts = new double[net.getLayer(layerId - 1).getSize()];
        for (int i = 0; i < wts.length; i++)
            wts[i] = net.getLink(layerId - 1).getWeight(i, neuronId);
        return wts;
    }

    /**
     * Get output weights of some neuron
     */
    public double[] getOutWeights(int layerId, int neuronId) {
        if(layerId < 0 || layerId >= net.getN())
            throw new ANNException("please provide a suitable layer id");
        if(neuronId < 0 || neuronId > net.getLayer(layerId).getSize())
            throw new ANNException("please provide a suitable neuron id");

        double[] wts = new double[net.getLayer(layerId).getSize()];
        for (int i = 0; i < wts.length; i++)
            wts[i] = net.getLink(layerId).getWeight(neuronId, i);
        return wts;
    }
}
