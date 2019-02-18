import java.util.Arrays;

public class Test {
    public static void main(String[] args) throws Exception {
        ANN net = new ANN("./ExampleNN.xml");
        for (int j = 0; j < net.net.getN(); j++) {
            System.out.println(String.format("=== %d ===", j));
            try {
                for (int i = 0; i == i; i++) {
                    System.out.println(i + ": " + Arrays.toString(Arrays.stream(net.getInWeights(j, i)).map(Math::round).toArray()));
                }
            } catch (ANNException e) {
            }
        }
        for (int j = 0; j < net.net.getN(); j++) {
            System.out.println(String.format("=== %d ===", j));
            try {
                for (int i = 0; i == i; i++) {
                    System.out.println(i + ": " + Arrays.toString(Arrays.stream(net.getOutWeights(j, i)).map(Math::round).toArray()));
                }
            } catch (ANNException e) {
            }
        }
    }
}
