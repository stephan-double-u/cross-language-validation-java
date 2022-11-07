package de.swa.clv.constraints;

import de.swa.clv.json.JsonSerializable;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

public abstract class Constraint extends ConstraintRoot {

    public static final String EMPTY_VALUES_ERR_MESSAGE = "Provide at least one value";
    public static final String NUMBERS_TYPE_ERR_MESSAGE = "Numbers must have same type";

    // defines if a null object should be evaluated to true or false
    private boolean nullEqualsTrue = false;

    Constraint() {
    }

    /**
     * Validates the constraint values and the propertyType.
     *
     * @param propertyType TODO
     * @return {@code true} if the values and propertyType are o.k., {@code false} otherwise
     */
    @SuppressWarnings("squid:S1172")

    public void validateValuesOrFail(final Class<?> typeClass, final Class<?> propertyType) {
    }

    public boolean doesNullEqualsTrue() {
        return nullEqualsTrue;
    }

    public void setNullEqualsTrue(boolean nullEqualsTrue) {
        this.nullEqualsTrue = nullEqualsTrue;
    }

    static void assertValuesAndSizeOk(Object[] values) {
        if (values == null || Arrays.asList(values).contains(null)) {
            throw new IllegalArgumentException(NULL_VALUE_ERR_MESSAGE);
        }
        if (values.length == 0) {
            throw new IllegalArgumentException(EMPTY_VALUES_ERR_MESSAGE);
        }
    }

    List<Object> getValuesAsObjectList(Object[] values) {
        return new ArrayList<>(Arrays.asList(values));
    }

    List<Object> getValuesWithAllowFlagAsObjectList(boolean nullEqualsTrue, Object[] values) {
        List<Object> list = new ArrayList<>(Arrays.asList(values));
        list.add(0, nullEqualsTrue);
        return list;
    }

    static void assertNumbersHaveSameType(Number[] values) {
        Class<? extends Number> numberClass = values[0].getClass();
        if (!Arrays.stream(values).allMatch(v -> numberClass == v.getClass())) {
            throw new IllegalArgumentException(NUMBERS_TYPE_ERR_MESSAGE);
        }
    }


}
