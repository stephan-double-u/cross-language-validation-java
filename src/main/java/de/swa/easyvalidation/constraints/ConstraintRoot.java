package de.swa.easyvalidation.constraints;

import de.swa.easyvalidation.json.JsonSerializable;

import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

public abstract class ConstraintRoot implements JsonSerializable {

    // Holds any constraint related values
    private List<Object> values;

    ConstraintRoot() {
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
     * @param propertyType
     * @return {@code true} if the values and propertyType are o.k., {@code false} otherwise
     */
    public boolean validateValuesOrFail(final Class<?> propertyType) throws IllegalArgumentException {
        return true;
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

}
