package de.swa.clv.constraints;

import de.swa.clv.json.JsonSerializable;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

public abstract class ConstraintRoot implements JsonSerializable {

    public static final String NULL_VALUE_ERR_MESSAGE = "Null values are not allowed";

    // holds constraint specific values
    private List<Object> values;

    ConstraintRoot() {
    }

    public abstract String getToken();

    /**
     * Checks if the class is supported by the constraint.
     *
     * @param clazz
     *            the class to check
     * @return {@code true} if the class is supported, {@code false} otherwise
     */
    public abstract boolean isSupportedType(Class<?> clazz);

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
    void setValues(final List<Object> values) {
        this.values = Collections.unmodifiableList(values);
    }

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

}
