package de.swa.easyvalidation.constraints;
import static de.swa.easyvalidation.json.JsonUtil.asArray;
import static de.swa.easyvalidation.json.JsonUtil.asKey;
import static de.swa.easyvalidation.json.JsonUtil.asObject;
import static de.swa.easyvalidation.json.JsonUtil.quoted;

import java.sql.JDBCType;
import java.time.LocalDate;
import java.util.Arrays;
import java.util.Objects;

import de.swa.easyvalidation.util.TypeHelper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public abstract class Equals extends Constraint {

    private static Logger log = LoggerFactory.getLogger(Equals.class);

    Equals() {
    }

    abstract String getType();

    // Enum.FOO should be equal to "FOO" and vice versa
    static boolean equals(Object valueToValidate, Object referencedValue) {
        if (valueToValidate == null && referencedValue == null) {
            return true;
        }
        if (valueToValidate == null || referencedValue == null) {
            return false;
        }
        if (Enum.class.isAssignableFrom(valueToValidate.getClass())) {
            valueToValidate = ((Enum) valueToValidate).name();
        }
        if (Enum.class.isAssignableFrom(referencedValue.getClass())) {
            referencedValue = ((Enum) referencedValue).name();
        }
        return valueToValidate.equals(referencedValue);
    }

    /*
     * Methods that create EqualsAny constraints
     */
    
    public static EqualsAny any(final String... values) {
        if (Arrays.asList(values).contains(null)){
            throw new IllegalArgumentException("Null values are not allowed");
        }
        return new EqualsAny(values);
    }
    
    public static EqualsAny any(final Enum<?>... values) {
        if (Arrays.asList(values).contains(null)){
            throw new IllegalArgumentException("Null values are not allowed");
        }
        return new EqualsAny(values);
    }

    public static EqualsAny any(final Number... values) {
        if (Arrays.asList(values).contains(null)){
            throw new IllegalArgumentException("Null values are not allowed");
        }
        return new EqualsAny(values);
    }

    public static EqualsAny any(final Boolean... values) {
        if (Arrays.asList(values).contains(null)){
            throw new IllegalArgumentException("Null values are not allowed");
        }
        return new EqualsAny(values);
    }

    public static EqualsAny any(final LocalDate... values) {
        if (Arrays.asList(values).contains(null)){
            throw new IllegalArgumentException("Null values are not allowed");
        }
        return new EqualsAny(values);
    }

    /*
     * Methods that create EqualsNone constraints
     */
    
    public static EqualsNone none(final String... values) {
        if (Arrays.asList(values).contains(null)){
            throw new IllegalArgumentException("Null values are not allowed");
        }
        return new EqualsNone(values);
    }
    
    public static EqualsNone none(final Enum<?>... values) {
        if (Arrays.asList(values).contains(null)){
            throw new IllegalArgumentException("Null values are not allowed");
        }
        return new EqualsNone(values);
    }

    public static EqualsNone none(final Number... values) {
        if (Arrays.asList(values).contains(null)){
            throw new IllegalArgumentException("Null values are not allowed");
        }
        return new EqualsNone(values);
    }

    public static EqualsNone none(final Boolean... values) {
        if (Arrays.asList(values).contains(null)){
            throw new IllegalArgumentException("Null values are not allowed");
        }
        return new EqualsNone(values);
    }

    public static EqualsNone none(final LocalDate... values) {
        if (Arrays.asList(values).contains(null)){
            throw new IllegalArgumentException("Null values are not allowed");
        }
        return new EqualsNone(values);
    }

    /*
     * Null related methods
     */

    public static EqualsNull null_() {
        return new EqualsNull();
    }

    public static EqualsNotNull notNull() {
        return new EqualsNotNull();
    }

    /*
     * Methods that create Equals*Ref constraints
     */
    
    public static EqualsAnyRef anyRef(final String... values) {
        if (Arrays.asList(values).contains(null)){
            throw new IllegalArgumentException("Null values are not allowed");
        }
        return new EqualsAnyRef(values);
    }
    
    public static EqualsNoneRef noneRef(final String... values) {
        if (Arrays.asList(values).contains(null)){
            throw new IllegalArgumentException("Null values are not allowed");
        }
        return new EqualsNoneRef(values);
    }
    
    
    @Override
    public boolean isSupportedType(Class<?> clazz) {
        if (clazz.isPrimitive()) {
            clazz = TypeHelper.PRIMITIVE_TO_WRAPPER_TYPES.get(clazz);
        }
        return String.class == clazz // String is final
                || Enum.class.isAssignableFrom(clazz)
                || Number.class.isAssignableFrom(clazz) 
                || Boolean.class == clazz // Boolean is final
                || LocalDate.class == clazz;
    }
    
    @Override
    public String serializeToJson() {
        String type = getType();
        String valuesJson = getValues() != null ? "," + asKey("values") + asArray(getValues()) : "";
        return asKey("type") + quoted(type) + valuesJson;
    }

}
