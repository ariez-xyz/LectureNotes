import assignment11_int.Assignment11;
import assignment11_int.Matrix;

import java.io.*;
import java.util.ArrayList;
import java.util.Arrays;

public class NeuralNetwork implements Assignment11 {
    // in, hidden and output neuron count
    private int inputSize;
    private int hiddenNeuronsCount;
    private int outputNeuronsCount;

    private double learningrate;

    private ArrayList<Matrix> data = new ArrayList<>();
    private ArrayList<Integer> labels = new ArrayList<>();

    // Weights[i][j] is link from neuron i in current layer to neuron j in next layer
    private Matrix inToHiddenWeights;
    private Matrix hiddenToOutWeights;

    /**
     * Create a new neural network with an input, hidden and output layer of specified dimensions
     * Weights are random in [-0.5, 0.5) by default
     * @param inputSize Number of neurons in input layer
     * @param hiddenNeuronsCount Number of neurons in hidden layer
     * @param outputNeuronsCount Number of neurons in output layer
     * @param learningrate Learning rate
     */
    public NeuralNetwork(int inputSize, int hiddenNeuronsCount, int outputNeuronsCount, double learningrate) {
        this.inputSize = inputSize;
        this.hiddenNeuronsCount = hiddenNeuronsCount;
        this.outputNeuronsCount = outputNeuronsCount;

        this.learningrate = learningrate;

        inToHiddenWeights = new Matrix(hiddenNeuronsCount, inputSize, () -> Math.random() - 0.5);
        hiddenToOutWeights = new Matrix(outputNeuronsCount, hiddenNeuronsCount, () -> Math.random() - 0.5);
    }

    /**
     * Parse training data from CSV file and prepare for training
     * @param csvTrainingData CSV training examples with label as first value in each line
     * @return true on success
     */
    @Override
    public boolean init(File csvTrainingData) {
        try (BufferedReader br = new BufferedReader(new FileReader(csvTrainingData))) {
            String line;
            int linect = 0;
            long beginTime = System.currentTimeMillis();
            while ((line = br.readLine()) != null) {
                if(line.split(",").length != inputSize + 1) {
                    System.err.println(String.format("[WARNING] skipping bad training example in csv file %s (line %d): expected input size: %d + 1 values, got %d", csvTrainingData.getName(), linect, inputSize, line.split(",").length));
                    continue;
                }
                labels.add(line.charAt(0) - 48);
                data.add(unlabeledDigitToMatrix(line.substring(2)));
                linect++;
                System.out.print("parsing training data... (processed " + linect + " samples)\r");
            }
            System.out.println(String.format("\ndone (%ds)", (System.currentTimeMillis() - beginTime) / 1000));
        } catch (Throwable e) {
            e.printStackTrace();
            return false;
        }
        return true;
    }

    /**
     * Train network repeatedly
     * @param epochs Number of times to train the network
     * @return true on success
     */
    public boolean train(int epochs) {
        long beginTime = System.currentTimeMillis();
        for (int i = 0; i < epochs; i++) {
            System.out.println("### epoch " + (i + 1));
            if(!train())
                return false;
        }
        System.out.printf("### finished training (%d epoch%s in %ds)\n", epochs, epochs == 1 ? "" : "s", (System.currentTimeMillis() - beginTime) / 1000);
        return true;
    }

    /**
     * Train network with training data previously given in init()
     * @return true on success
     */
    @Override
    public boolean train() {
        if(data.size() == 0) {
            System.err.println("[ERROR] attempting to train with empty dataset");
            return false;
        }
        long beginTime = System.currentTimeMillis();
        for (int i = 0; i < data.size(); i++) {
            Matrix inputs = data.get(i);
            double[] desiredOutput = new double[outputNeuronsCount];
            Arrays.fill(desiredOutput, 0.01);
            desiredOutput[labels.get(i)] = 0.99;
            Matrix target = new Matrix(desiredOutput).getTranspose();

            Matrix hiddenInputs = inToHiddenWeights.getDotProduct(inputs);
            Matrix hiddenOutputs = hiddenInputs.apply(NeuralNetwork::sigmoid);

            Matrix finalInputs = hiddenToOutWeights.getDotProduct(hiddenOutputs);
            Matrix finalOutputs = finalInputs.apply(NeuralNetwork::sigmoid);

            Matrix outputErrors = target.minus(finalOutputs);
            Matrix hiddenErrors = hiddenToOutWeights.getTranspose().getDotProduct(outputErrors);

            hiddenToOutWeights.plus(outputErrors.mult(finalOutputs).mult(finalOutputs.apply(d -> 1-d)).getDotProduct(hiddenOutputs.getTranspose()).apply(d -> d * learningrate));
            inToHiddenWeights.plus(hiddenErrors.mult(hiddenOutputs).mult(hiddenOutputs.apply(d -> 1-d)).getDotProduct(inputs.getTranspose()).apply(d -> d * learningrate));

            System.out.print("training net... (processed " + (i + 1) + " samples)\r");
        }
        System.out.println(String.format("\ndone (%ds)", (System.currentTimeMillis() - beginTime) / 1000));
        return true;
    }

    /**
     * Recognize CSV training example
     * @param csvString The training example
     * @return Index of neuron with largest output in the output layer
     */
    @Override
    public int recognize(String csvString) {
        Matrix input = unlabeledDigitToMatrix(csvString);
        Matrix hidden = inToHiddenWeights.getDotProduct(input).apply(NeuralNetwork::sigmoid);
        Matrix output = hiddenToOutWeights.getDotProduct(hidden).apply(NeuralNetwork::sigmoid);
        return output.getMaxColIdx();
    }

    /**
     * turn MNIST csv string into a matrix and map values to interval [0.01, 1]
     * returns a 784x1 matrix
     * @param csvString of form 0,0,0,0,124,255,...
     * @return
     */
    private Matrix unlabeledDigitToMatrix(String csvString) {
        return new Matrix(Arrays.stream(csvString.split(",")).mapToDouble(Double::parseDouble).map(n -> (n / 255) * 0.99 + 0.01).toArray()).getTranspose();
    }

    private static double sigmoid(double x) {
        return (0.99 / (1 + Math.exp(-x))) + 0.01;
    }

    // equivalent to unlabeledDigitToMatrix
    // not much faster though... disk i/o seems to be bottleneck
    private Matrix experimentalFastParse(String csvString) {
        double[] vals = new double[784];
        double current = 0;
        int count = 0;
        for (int i = 2; i < csvString.length(); i++) {
            if(csvString.charAt(i) == ',') {
                vals[count++] = (current * 0.99 / 255) + 0.01;
                current = 0;
            } else {
                current *= 10;
                current += csvString.charAt(i) - 48;
            }
        }
        return new Matrix(vals).getTranspose();
    }
}
