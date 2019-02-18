package Functions;

public class StepFunction implements ActivationFunction {
    private double cutoff;

    public StepFunction(double cutoff) {
        this.cutoff = cutoff;
    }

    @Override
    public double calc(double weightedInput) {
        return weightedInput > 0.5 ? 1 : 0;
    }

    public String toString() {
        return "step " + cutoff;
    }
}
