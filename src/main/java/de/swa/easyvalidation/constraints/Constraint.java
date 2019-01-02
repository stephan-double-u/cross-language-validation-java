package de.swa.easyvalidation.constraints;

import java.util.ArrayList;
import java.util.List;

import de.swa.easyvalidation.json.JsonSerializable;

public abstract class Constraint implements JsonSerializable {

    private List<Object> values;

    protected Constraint() {
        super();
        this.values = new ArrayList<>();
    }

    public static ConstraintRef ref(String propertyName, Constraint constraint) {
        return new ConstraintRef(propertyName, constraint);
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
     * Validates the given value against this constraint. Null values are validated to true.
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
    public boolean validateArgumentsOrFail(Class<?> propertyType) {
        return true;
    }
    
    public List<Object> getValues() {
        return values;
    }

    /*
     * Setter methods for the different kinds of constraint values.
     */
    
    public void setBooleanValue(boolean value) {
        this.values.add(value);
    }

    public void setStringValues(List<String> values) {
        for (String value : values) {
            this.values.add(value);
        }
    }

    public void setEnumValues(List<Enum<?>> values) {
        for (Enum<?> value : values) {
            this.values.add(value);
        }
    }

    public void setNumberValues(List<Number> values) {
        for (Number value : values) {
            this.values.add(value);
        }
    }

    public void setObjectValues(List<Object> values) {
        this.values = values;
    }

}
