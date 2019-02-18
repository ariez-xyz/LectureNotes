package pape_sismanovic;

import exceptions.IllegalInputException;
import exceptions.SizeLimitReachedException;
import exceptions.StackNotSetException;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.aggregator.ArgumentsAccessor;
import org.junit.jupiter.params.provider.CsvSource;


import java.util.Arrays;

import static org.junit.jupiter.api.Assertions.*;

class RPN_CalculatorTest {

    private RPN_Calculator c;

    @BeforeEach
    void setUp() {
        c = new RPN_Calculator();
        // run tests w/ ArrayStack by default - should be fine, both stacks are tested
        c.setStack(new ArrayStack<>());
    }

    @Test
    @DisplayName("setStack() test")
    void setStack() throws Exception {
        c = new RPN_Calculator();
        String[] testInput = new String[2001];
        Arrays.setAll(testInput, p -> p > 1000 ? "+" : p + ""); // 0 + 1 + 2 + ... + 1000 = 500500

        assertThrows(StackNotSetException.class, () -> c.calc(testInput), "null stack");

        c.setStack(new ArrayStack<>());
        assertThrows(SizeLimitReachedException.class, () -> c.calc(testInput), "input too large for array");

        c.setStack(new LinkedListStack<>());
        assertEquals(500500, c.calc(testInput));
    }

    @ParameterizedTest
    @CsvSource({
            // Format: "RESULT, RPN EXPRESSION ..."
            "0, 0, 7, /",
            "0, 5, 0, *",
            "0, 0, 5, *",
            "0, 0, 0, *",
            "10, 3, 7, +",
            "10, 7, 3, +",
            "5, 11, 6, -",
            "-5, 6, 11, -",
            "54, 3, 18, *",
            "54, 18, 3, *",
            "0.4, 4, 10, /",
            "2.5, 10, 4, /",
            "6.25, 10, 8, /, 5, *",
            "0.5, 10, 4, /, 5, *, 10, -, 5, /",
            "12, 8, 3, *, 2, /",
            "13, 4, 3, *, 12, *, 25, +, 13, /"
    })
    void testArithmetic(ArgumentsAccessor args) throws Exception {
        String[] calcs = (String[]) args.toArray();
        assertEquals((double) args.getDouble(0), c.calc(Arrays.copyOfRange(calcs, 1, calcs.length)));
    }

    @ParameterizedTest
    @CsvSource({
            "+",
            ", 4",
            ",",
            "*",
            "3, 4",
            "this is not a number",
            "5, 5, +, +",
            "4, -",
            "3, 4, +, test cases w/ results from above follow:",
            "10, 3, 7, +",
            "10, 7, 3, +",
            "5, 11, 6, -",
            "-5, 6, 11, -",
            "54, 3, 18, *",
            "54, 18, 3, *",
            "0.4, 4, 10, /",
            "2.5, 10, 4, /"
    })
    void testBadInput(ArgumentsAccessor args) {
        String[] in = (String[]) args.toArray();
        assertThrows(IllegalInputException.class, () -> c.calc(in));
    }
}
