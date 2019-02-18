import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;

public class DigitRecognizer {
    private static void usage() {
        System.err.println("usage: java DigitRecognizer <trainset> <testset> <learningrate> <epochs> <hiddenNeurons>");
    }

    public static void main(String[] args) throws Exception {
        if(args.length != 5) {
            usage();
            return;
        }

        String pathToTrainSet = args[0];
        String pathToTestSet = args[1];
        double learningrate = Double.parseDouble(args[2]);
        int epochs = Integer.parseInt(args[3]);
        int hiddenNeurons = Integer.parseInt(args[4]);

        NeuralNetwork nn = new NeuralNetwork(784, hiddenNeurons, 10, learningrate);
        nn.init(new File(pathToTrainSet));
        nn.train(epochs);

        try (BufferedReader br = new BufferedReader(new FileReader(pathToTestSet))) {
            String line;
            int count = 0;
            int correct = 0;
            while((line = br.readLine()) != null) {
                int label = Integer.parseInt(line.substring(0, 1));
                int out = nn.recognize(line.substring(2));
                count++;
                if(label == out)
                    correct++;
                System.out.printf("measuring accuracy (current sample: %d)\r", count);
            }
            System.out.printf("\ndone. test set accuracy: %f%%\n", (100 * correct / (double) count));
        }
    }
}
