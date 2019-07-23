package de.swa.easyvalidation.constraints;

import de.swa.easyvalidation.json.JsonSerializable;

import java.util.ArrayList;
import java.util.List;

public abstract class ConstraintRoot implements JsonSerializable {

    private List<Object> values;

    protected ConstraintRoot() {
        super();
        values = new ArrayList<>();
    }

    /**
     * Checks if the class is supported by the contraint.
     * 
     * @param clazz
     *            the class to check
     * @return {@code true} if the class is supported, {@code false} otherwise
     */
    public abstract boolean isSupportedType(Class<?> clazz);
    
    /**
     * Validates the given value against this constraint.
     * 
     * @param value
     *            the value of to validate
     * @param contraintObject
     *            the object the constraint is defined on
     * @return {@code true} if the constraint is fulfilled, {@code false} otherwise
     */
    public abstract boolean validate(Object value, Object contraintObject);

    
    /**
     * Should be overwritten if a constraint has to do some validations.
     * @param propertyType 
     * 
     * @return
     */
    public boolean validateArgumentsOrFail(final Class<?> propertyType) {
        return true;
    }
    
    public List<Object> getValues() {
        return values;
    }

    /*
     * Setter methods for the different kinds of constraint values.
     */
    
    public void setBooleanValue(final boolean value) {
        values.add(value);
    }

    public void setStringValues(final List<String> values) {
        for (final String value : values) {
            this.values.add(value);
        }
    }

    public void setEnumValues(final List<Enum<?>> values) {
        for (final Enum<?> value : values) {
            this.values.add(value);
        }
    }

    public void setNumberValues(final List<Number> values) {
        for (final Number value : values) {
            this.values.add(value);
        }
    }

    public void setObjectValues(final List<Object> values) {
        this.values = values;
    }

}
