package pape_sismanovic;

import assignment2_int.Stack;
import exceptions.EmptyStackException;
import exceptions.SizeLimitReachedException;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.aggregator.ArgumentsAccessor;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.MethodSource;

import java.util.LinkedList;
import java.util.stream.IntStream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

class StackTest {

    private static LinkedList<Stack<Object>> stacks;

    @BeforeEach
    void setUp() {
         stacks = new LinkedList<>();
         stacks.add(new LinkedListStack<>());
         stacks.add(new ArrayStack<>());
    }

    @ParameterizedTest
    @CsvSource({
            // Format: "NUM_OF_ELS_TO_POP, EXPECTED_ON_TOP, ELS ..."
            "0, a, a",
            "0, b, a, b",
            "2, a, a, b, c",
            "3, b, a, b, c, d, e",
            "7, d, a, b, c, d, e, f, g, h, i, j, k",
    })
    void stackOperations(ArgumentsAccessor args) throws Exception {
        String[] in = (String[]) args.toArray();
        int pop = args.getInteger(0);
        String expected = args.getString(1);

        for(Stack<Object> s : stacks) {
            assertThrows(EmptyStackException.class, s::pop);

            for (int i = 2; i < in.length; i++)
                s.push(in[i]);

            for (int i = 0; i < pop; i++)
                s.pop();

            assertEquals(expected, s.pop());
        }
    }

    @ParameterizedTest
    @MethodSource("range")
    void size(int max) throws Exception {
        for (Stack<Object> s : stacks) {
            for (int i = 0; i < max; i++)
                s.push("");
            assertEquals(max, s.size());
        }
    }

    static IntStream range() {
        return IntStream.range(0, 100);
    }

    @Test
    void peek() throws Exception {
        for (Stack<Object> s : stacks) {
            assertThrows(EmptyStackException.class, s::peek);
            s.push("1");
            s.push("2");
            s.push("3");
            s.push("4");
            int size = s.size();
            assertEquals("4", s.peek());
            assertEquals(size, s.size());
            assertEquals("4", s.peek());
        }
    }

    @Test
    void testEdgecases() throws Exception {
        for (Stack<Object> s : stacks) {
            assertThrows(EmptyStackException.class, s::pop);
            assertThrows(EmptyStackException.class, s::peek);

            if (s instanceof ArrayStack) {
                for (int i = 0; i < ArrayStack.ARRAY_SIZE; i++)
                    s.push("...");
                assertThrows(SizeLimitReachedException.class, () -> s.push("should be too much"));
            }
        }
    }
}