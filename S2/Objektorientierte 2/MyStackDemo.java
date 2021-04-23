//David Pape 01634454
//Johannes Spilka 11724817
//Filip Vecek 11700962

import java.util.Arrays;
import java.util.concurrent.ThreadLocalRandom;

public class MyStackDemo {
    //(POOL_SIZE / STEP_SIZE) must be an integer
    private final static int POOL_SIZE = 500;
    private final static int STEP_SIZE = 5;
    private final static int ARRAY_LENGTH = POOL_SIZE / STEP_SIZE;
    private final static int NUM_OF_OPERATIONS = 1000;
    private final static boolean LOG_FULL_DATA = false;

    public static void main(String[] args) {
        print("Generating pool of " + POOL_SIZE + " elements...");
        char[] pool = new char[POOL_SIZE];
        for (int i = 0; i < POOL_SIZE; i++)
            pool[i] = Character.toChars(ThreadLocalRandom.current().nextInt(33, 127))[0];

        print("Length of randomized arrays: " + ARRAY_LENGTH);
        String[] randomStrings = new String[ARRAY_LENGTH];
        SHA256[] randomSHA256 = new SHA256[ARRAY_LENGTH];
        Double[] randomDoubles = new Double[ARRAY_LENGTH];

        print("Filling arrays with data from pool...");
        for (int i = 0; i < POOL_SIZE; i += STEP_SIZE) {
            String s = "";
            Integer i1 = 0;
            Integer i2 = 0;

            for (int j = 0; j < STEP_SIZE; j++) {
                s += pool[i + j];

                if (j < STEP_SIZE / 2)
                    i1 += pool[i + j];
                else
                    i2 += pool[i + j];
            }

            randomStrings[i / STEP_SIZE] = s;
            randomSHA256[i / STEP_SIZE] = new SHA256(s);
            randomDoubles[i / STEP_SIZE] = Math.pow(i1, ((double) i1 / i2));
        }

        print("Created arrays:");
        print(Arrays.toString(randomDoubles));
        print(Arrays.toString(randomSHA256));
        print(Arrays.toString(randomStrings));

        print("Creating and filling stacks...");
        Stack[] stacks = new Stack[6];
        stacks[0] = new StackDynamic<>(randomSHA256);
        stacks[1] = new StackDynamic<>(randomDoubles);
        stacks[2] = new StackDynamic<>(randomStrings);
        stacks[3] = new StackArray<>(randomSHA256);
        stacks[4] = new StackArray<>(randomDoubles);
        stacks[5] = new StackArray<>(randomStrings);

        print("Created stacks:");
        print(stacks);

        int pushCount = 0;
        int popCount = 0;
        int peekCount = 0;

        print("Starting random operations...");
        for (int i = 0; i < NUM_OF_OPERATIONS; i++) {
            int rand = ThreadLocalRandom.current().nextInt(0, 5) % 3;

            switch (rand) {
                case 0:
                    print("pushing...");
                    pushCount++;
                    int randIndex = ThreadLocalRandom.current().nextInt(0, ARRAY_LENGTH);

                    for (int j = 0; j < 6; j++)
                        if (j % 3 == 0)
                            stacks[j].push(randomSHA256[randIndex]);
                        else if (j % 3 == 1)
                            stacks[j].push(randomDoubles[randIndex]);
                        else
                            stacks[j].push(randomStrings[randIndex]);

                    print("Updated stacks: ");
                    print(stacks);
                    break;
                case 1:
                    print("popping...");
                    popCount++;
                    for(Stack s : stacks)
                        print("\tpopped " + s.pop() + " from " + s.getClass().getName());

                    print("Updated stacks: ");
                    print(stacks);
                    break;
                case 2:
                    print("peeking...");
                    peekCount++;
                    for(Stack s : stacks)
                        print("\tpeeked into " + s.getClass().getName() + " with result " + s.peek());

                    break;
            }
        }

        print("done...");
        print("pushed " + pushCount + " times, popped " + popCount + " times, peeked " + peekCount + "times");

    }

    private static void print(Stack[] stacks) {
        for(Stack s : stacks)
            print("\tType: " + s.getClass().getName() + " with contents " + s.toString());
    }

    public static void print(String s) {
        if(LOG_FULL_DATA)
            System.out.println(s);
        else if (s.length() > 250)
            System.out.println(s.substring(0, 201) + "... (" + (s.length() - 220) + " more) ..." + s.substring(s.length() - 20, s.length()));
        else
            System.out.println(s);
    }
}
