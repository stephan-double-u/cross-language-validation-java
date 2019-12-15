package de.swa.easyvalidation.constraints;

import java.time.LocalDate;
import java.util.Arrays;

/**
 * Provides static methods to create all different kinds of equals constraints.
 */
public class Equals {

    private Equals() {
    }

    /*
     * Methods that create EqualsAny constraints
     */
    
    public static EqualsAny any(final String... values) {
        if (Arrays.asList(values).contains(null)) {
            throw new IllegalArgumentException("Null values are not allowed");
        }
        return new EqualsAny(values);
    }
    
    public static EqualsAny any(final Enum<?>... values) {
        if (Arrays.asList(values).contains(null)) {
            throw new IllegalArgumentException("Null values are not allowed");
        }
        return new EqualsAny(values);
    }

    public static EqualsAny any(final Number... values) {
        if (Arrays.asList(values).contains(null)) {
            throw new IllegalArgumentException("Null values are not allowed");
        }
        return new EqualsAny(values);
    }

    public static EqualsAny any(final Boolean... values) {
        if (Arrays.asList(values).contains(null)) {
            throw new IllegalArgumentException("Null values are not allowed");
        }
        return new EqualsAny(values);
    }

    public static EqualsAny any(final LocalDate... values) {
        if (Arrays.asList(values).contains(null)) {
            throw new IllegalArgumentException("Null values are not allowed");
        }
        return new EqualsAny(values);
    }

    /*
     * Methods that create EqualsNone constraints
     */
    
    public static EqualsNone none(final String... values) {
        if (Arrays.asList(values).contains(null)) {
            throw new IllegalArgumentException("Null values are not allowed");
        }
        return new EqualsNone(values);
    }
    
    public static EqualsNone none(final Enum<?>... values) {
        if (Arrays.asList(values).contains(null)) {
            throw new IllegalArgumentException("Null values are not allowed");
        }
        return new EqualsNone(values);
    }

    public static EqualsNone none(final Number... values) {
        if (Arrays.asList(values).contains(null)) {
            throw new IllegalArgumentException("Null values are not allowed");
        }
        return new EqualsNone(values);
    }

    public static EqualsNone none(final Boolean... values) {
        if (Arrays.asList(values).contains(null)) {
            throw new IllegalArgumentException("Null values are not allowed");
        }
        return new EqualsNone(values);
    }

    public static EqualsNone none(final LocalDate... values) {
        if (Arrays.asList(values).contains(null)) {
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
        if (Arrays.asList(values).contains(null)) {
            throw new IllegalArgumentException("Null values are not allowed");
        }
        return new EqualsAnyRef(values);
    }
    
    public static EqualsNoneRef noneRef(final String... values) {
        if (Arrays.asList(values).contains(null)) {
            throw new IllegalArgumentException("Null values are not allowed");
        }
        return new EqualsNoneRef(values);
    }
    
}
