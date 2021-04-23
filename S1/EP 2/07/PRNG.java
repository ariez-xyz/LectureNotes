import java.util.Random;
public class PRNG {
    private static Random prng = new Random();
    private static char[] alphabet = {'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j',
	'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z',
	'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P',
	'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', '0', '1', '2', '3', '4', '5',
	'6', '7', '8', '9'};
    public static void randomize(long seed) {
	prng.setSeed(seed);
    }
    public static String randomString(int length, char[] alphabet) {
	StringBuilder sb = new StringBuilder(length);
	for (int i = 0; i < length; i++)
	    sb.append(alphabet[prng.nextInt(alphabet.length)]);
	return sb.toString();
    }
    public static String randomString() {
	return randomString(10 + randomInt(20), alphabet);
    }
    public static int randomInt() {
	return prng.nextInt();
    }
    public static int randomInt(int size) {
	return prng.nextInt(size);
    }
    public static double randomDouble() {
	return prng.nextDouble();
    }
}

