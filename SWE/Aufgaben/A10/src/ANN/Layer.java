package ANN;

import Functions.ActivationFunction;

public class Layer {
    private ActivationFunction f;
    private Neuron[] neurons;
    private int n;

    public Layer(int size, ActivationFunction f) {
        this.f = f;
        this.n = size;
        neurons = new Neuron[n];
        for (int i = 0; i < n; i++)
            neurons[i] = new Neuron(f);
    }

    public double[] think(double[] food) {
        double[] thought = new double[neurons.length];
        for (int i = 0; i < neurons.length; i++)
            thought[i] = neurons[i].think(food[i]);
        return thought;
    }

    public void setActivationFunction(int whichNeuron, ActivationFunction newFunction) {
        neurons[whichNeuron].thinkDifferent(newFunction);
    }

    public void setActivationFunction(ActivationFunction newFunction) {
        this.f = newFunction;
    }

    public int getSize() {
        return n;
    }
}
