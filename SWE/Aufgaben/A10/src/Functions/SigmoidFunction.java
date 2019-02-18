package Functions;

public class SigmoidFunction implements ActivationFunction {
    @Override
    public double calc(double weightedInput) {
        return 1 / (1 + Math.pow(Math.E, -weightedInput));
    }

    // TODO add tostring

}
