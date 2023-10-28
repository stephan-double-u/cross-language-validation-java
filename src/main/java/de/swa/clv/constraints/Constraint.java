package de.swa.clv.constraints;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static de.swa.clv.json.JsonUtil.asKey;
import static de.swa.clv.json.JsonUtil.quoted;

public abstract class Constraint extends ConstraintRoot {

    public static final String EMPTY_VALUES_ERR_MESSAGE = "Provide at least one value";
    public static final String NUMBERS_TYPE_ERR_MESSAGE = "Numbers must have same type";

    // defines if a null object should be evaluated to true or false
    private boolean nullEqualsTrue = false;

    Constraint() {
    }

    @SuppressWarnings("squid:S1172")
    public void validateValuesOrFail(final Class<?> typeClass, final Class<?> propertyType) {
    }

    public boolean doesNullEqualsTrue() {
        return nullEqualsTrue;
    }

    public void setNullEqualsTrue(boolean nullEqualsTrue) {
        this.nullEqualsTrue = nullEqualsTrue;
    }

    public static void assertValuesAndSizeOk(Object[] values) {
        assertNotNull(values);
        assertNotEmpty(values);
    }

    public static void assertNotNull(Object[] values) {
        if (values == null || Arrays.asList(values).contains(null)) {
            throw new IllegalArgumentException(NULL_VALUE_ERR_MESSAGE);
        }
    }

    public static void assertNotEmpty(Object[] values) {
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

    String serializeRefTargetKeyValuePairForReferenceProperties() {
        if (this instanceof ReferenceProperties<?> refConstraint) {
            if (refConstraint.isOfCurrent()) {
                return "," + asKey("refTarget") + quoted("CURRENT_ENTITY");
            } else if (refConstraint.isOfUpdate()) {
                return "," + asKey("refTarget") + quoted("UPDATE_ENTITY");
            }
        }
        return "";
    }

    String getJsonForNullEqualsTrue(boolean defaultIsTrue) {
        return nullEqualsTrue != defaultIsTrue ? "," + asKey("nullEqualsTo") + nullEqualsTrue : "";
    }
}
