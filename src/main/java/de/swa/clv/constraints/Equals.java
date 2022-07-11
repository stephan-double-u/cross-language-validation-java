package de.swa.clv.constraints;

import java.time.LocalDate;
import java.util.Arrays;
import java.util.Objects;

/**
 * Provides static methods to create all different kinds of equals constraints.
 */
public class Equals {

    static final String NULL_VALUES_MESSAGE = "Null values are not allowed";
    static final String NUMBERS_CLASS_MESSAGE = "Numbers must have same type";

    private Equals() {
    }

    /*
     * Methods that create EqualsAny constraints
     */
    
    public static EqualsAny any(final String... values) {
        return new EqualsAny(values);
    }
    
    public static EqualsAny any(final Enum<?>... values) {
        return new EqualsAny(values);
    }

    public static EqualsAny any(final Number... values) {
        checkNumbersHaveSameTypeOfFail(values);
        return new EqualsAny(values);
    }

    public static EqualsAny any(final Boolean... values) {
        return new EqualsAny(values);
    }

    public static EqualsAny any(final LocalDate... values) {
        return new EqualsAny(values);
    }

    /*
     * Methods that create EqualsNone constraints
     */
    
    public static EqualsNone none(final String... values) {
        return new EqualsNone(values);
    }
    
    public static EqualsNone none(final Enum<?>... values) {
        return new EqualsNone(values);
    }

    public static EqualsNone none(final Number... values) {
        checkNumbersHaveSameTypeOfFail(values);
        return new EqualsNone(values);
    }

    public static EqualsNone none(final Long... values) {
        return new EqualsNone(values);
    }

    public static EqualsNone none(final Boolean... values) {
        return new EqualsNone(values);
    }

    public static EqualsNone none(final LocalDate... values) {
        return new EqualsNone(values);
    }

    /*
     * Null related methods
     */
    @SuppressWarnings("squid:S100")
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
        return new EqualsAnyRef(Objects.requireNonNullElseGet(values, () -> new String[] { null }));
    }
    
    public static EqualsNoneRef noneRef(final String... values) {
        return new EqualsNoneRef(Objects.requireNonNullElseGet(values, () -> new String[] { null }));
    }


    private static void checkNumbersHaveSameTypeOfFail(Number[] values) {
        Class<? extends Number> numberClass = values[0].getClass();
        if (!Arrays.stream(values).allMatch(v -> numberClass == v.getClass())) {
            throw new IllegalArgumentException(NUMBERS_CLASS_MESSAGE);
        }
    }

}
