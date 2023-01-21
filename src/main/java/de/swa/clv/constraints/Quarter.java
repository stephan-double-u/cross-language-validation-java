package de.swa.clv.constraints;

import java.util.Arrays;

public abstract class Quarter extends Dates {

    static final String QUARTERS_ERR_MESSAGE = "Values must be valid quarters, i.e. 1, 2, 3 or 4)";

    public static QuarterAny any(Integer... quarters) {
        assertValuesAndSizeOk(quarters);
        assertQuartersAreValid(quarters);
        return new QuarterAny(false, quarters);
    }

    public static QuarterAny anyOrNull(Integer ... quarters) {
        assertValuesAndSizeOk(quarters);
        assertQuartersAreValid(quarters);
        return new QuarterAny(true, quarters);
    }

    public static QuarterAnyRef anyRef(String ... properties) {
        assertValuesAndSizeOk(properties);
        return new QuarterAnyRef(false, properties);
    }

    public static QuarterAnyRef anyRefOrNull(String ... properties) {
        assertValuesAndSizeOk(properties);
        return new QuarterAnyRef(true, properties);
    }

    static void assertQuartersAreValid(Integer[] quarters) {
        if (!Arrays.stream(quarters).allMatch(q -> q >= 1 && q <= 4)) {
            throw new IllegalArgumentException(QUARTERS_ERR_MESSAGE);
        }
    }

}
