package ANN;

import Functions.ActivationFunction;

public class Neuron {
    private ActivationFunction f;

    public Neuron(ActivationFunction f) {
        this.f = f;
    }

    public double think(double weightedInput) {
        return f.calc(weightedInput);
    }

    public void thinkDifferent(ActivationFunction f) {
        this.f = f;
    }
}
