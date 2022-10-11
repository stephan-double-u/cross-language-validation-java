package de.swa.clv.constraints;

import de.swa.clv.json.JsonSerializable;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

public abstract class Constraint implements JsonSerializable {

    public static final String NULL_VALUE_ERR_MESSAGE = "Null values are not allowed";
    public static final String EMPTY_VALUES_ERR_MESSAGE = "Provide at least one value";
    public static final String NUMBERS_TYPE_ERR_MESSAGE = "Numbers must have same type";

    // Holds constraint specific values
    private List<Object> values;

    Constraint() {
    }

    public abstract String getType();

    /**
     * Checks if the class is supported by the constraint.
     * 
     * @param clazz
     *            the class to check
     * @return {@code true} if the class is supported, {@code false} otherwise
     */
    public abstract boolean isSupportedType(Class<?> clazz);
    
    /**
     * Validates the given object against this constraint.
     * 
     * @param value
     *            the object to validate
     * @param constraintObject
     *            the object the constraint is defined on
     * @return {@code true} if the constraint is fulfilled, {@code false} otherwise
     */
    public abstract boolean validate(Object value, Object constraintObject);


    /**
     * Validates the constraint values and the propertyType.
     *
     * @param propertyType TODO
     * @return {@code true} if the values and propertyType are o.k., {@code false} otherwise
     */
    @SuppressWarnings("squid:S1172")

    public void validateValuesOrFail(final Class<?> typeClass, final Class<?> propertyType) {
    }

    /**
     * Gets the constraint related values.
     *
     * @return the constraint related values
     */
    public List<Object> getValues() {
        return values;
    }

    /**
     * Sets the constraint related values.
     *
     * @param values the constraint related values
     */
    void setObjectValues(final List<Object> values) {
        this.values = Collections.unmodifiableList(values);
    }

    static void assertValuesAndSizeOk(Object[] values) {
        if (values == null || Arrays.asList(values).contains(null)) {
            throw new IllegalArgumentException(NULL_VALUE_ERR_MESSAGE);
        }
        if (values.length == 0) {
            throw new IllegalArgumentException(EMPTY_VALUES_ERR_MESSAGE);
        }
    }

    List<Object> getValuesWithAllowFlagAsObjectList(boolean nullEqualsTrue, Object[] values) {
        List<Object> list = new ArrayList<>(Arrays.asList(values));
        list.add(0, nullEqualsTrue);
        return list;
    }

}
