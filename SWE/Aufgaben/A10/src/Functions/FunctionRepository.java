package Functions;

import java.util.HashMap;

public class FunctionRepository {
    private static final double DEFAULT_STEP_CUTOFF = 0.5;
    private static final HashMap<String, ActivationFunction> repo = new HashMap<>();

    public static final String DEFAULT = "default";

    public static ActivationFunction get(String desc) {
        String[] splitDesc = desc.split(" ");
        try {
            switch (splitDesc[0]) {
                case "sigmoid":
                    return new SigmoidFunction();
                case "step":
                    return new StepFunction(splitDesc.length == 1 ? DEFAULT_STEP_CUTOFF : Double.parseDouble(splitDesc[1]));
                case "default":
                    return new StepFunction(DEFAULT_STEP_CUTOFF);
                default:
                    throw new UnknownFunctionException(String.format("Unknown activation function '%s'.", desc));
            }
        } catch (ArrayIndexOutOfBoundsException e) {
            throw new UnknownFunctionException(String.format("Bad activation function description: '%s'", desc));
        } catch (NumberFormatException e) {
            throw new UnknownFunctionException(String.format("Bad activation function parameter: '%s'", desc));
        }
    }

}
