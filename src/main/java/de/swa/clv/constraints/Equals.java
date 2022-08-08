package de.swa.clv.constraints;

import java.time.LocalDate;
import java.util.Arrays;

import static de.swa.clv.constraints.ConstraintRoot.*;

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
        assertValuesAndSizeOk(values);
        return new EqualsAny(false, values);
    }

    public static EqualsAny anyOrNull(final String... values) {
        assertValuesAndSizeOk(values);
        return new EqualsAny(true, values);
    }

    public static EqualsAny any(final Enum<?>... values) {
        assertValuesAndSizeOk(values);
        return new EqualsAny(false, values);
    }

    public static EqualsAny anyOrNull(final Enum<?>... values) {
        assertValuesAndSizeOk(values);
        return new EqualsAny(true, values);
    }

    public static EqualsAny any(final Number... values) {
        assertValuesAndSizeOk(values);
        assertNumbersHaveSameType(values);
        return new EqualsAny(false, values);
    }

    public static EqualsAny anyOrNull(final Number... values) {
        assertValuesAndSizeOk(values);
        assertNumbersHaveSameType(values);
        return new EqualsAny(true, values);
    }

    public static EqualsAny any(final Boolean... values) {
        assertValuesAndSizeOk(values);
        return new EqualsAny(false, values);
    }

    public static EqualsAny anyOrNull(final Boolean... values) {
        assertValuesAndSizeOk(values);
        return new EqualsAny(true, values);
    }

    public static EqualsAny any(final LocalDate... values) {
        assertValuesAndSizeOk(values);
        return new EqualsAny(false, values);
    }

    public static EqualsAny anyOrNull(final LocalDate... values) {
        assertValuesAndSizeOk(values);
        return new EqualsAny(true, values);
    }

    /*
     * Methods that create EqualsNone constraints
     */
    
    public static EqualsNone none(final String... values) {
        assertValuesAndSizeOk(values);
        return new EqualsNone(true, values);
    }
    
    public static EqualsNone noneNorNull(final String... values) {
        assertValuesAndSizeOk(values);
        return new EqualsNone(false, values);
    }

    public static EqualsNone none(final Enum<?>... values) {
        assertValuesAndSizeOk(values);
        return new EqualsNone(true, values);
    }

    public static EqualsNone noneNorNull(final Enum<?>... values) {
        assertValuesAndSizeOk(values);
        return new EqualsNone(false, values);
    }

    public static EqualsNone none(final Number... values) {
        assertValuesAndSizeOk(values);
        assertNumbersHaveSameType(values);
        return new EqualsNone(true, values);
    }

    public static EqualsNone noneNorNull(final Number... values) {
        assertValuesAndSizeOk(values);
        assertNumbersHaveSameType(values);
        return new EqualsNone(false, values);
    }

    public static EqualsNone none(final Long... values) {
        assertValuesAndSizeOk(values);
        return new EqualsNone(true, values);
    }

    public static EqualsNone none(final Boolean... values) {
        assertValuesAndSizeOk(values);
        return new EqualsNone(true, values);
    }

    public static EqualsNone noneNorNull(final Boolean... values) {
        assertValuesAndSizeOk(values);
        return new EqualsNone(false, values);
    }

    public static EqualsNone none(final LocalDate... values) {
        assertValuesAndSizeOk(values);
        return new EqualsNone(true, values);
    }

    public static EqualsNone noneNorNull(final LocalDate... values) {
        assertValuesAndSizeOk(values);
        return new EqualsNone(false, values);
    }

    /*
     * Methods that create Equals*Ref constraints
     */
    
    public static EqualsAnyRef anyRef(final String... values) {
        assertValuesAndSizeOk(values);
        return new EqualsAnyRef(false, values);
    }
    
    public static EqualsAnyRef anyRefOrNull(final String... values) {
        assertValuesAndSizeOk(values);
        return new EqualsAnyRef(true, values);
    }

    public static EqualsNoneRef noneRef(final String... values) {
        assertValuesAndSizeOk(values);
        return new EqualsNoneRef(true, values);
    }

    public static EqualsNoneRef noneRefNotNull(final String... values) {
        assertValuesAndSizeOk(values);
        return new EqualsNoneRef(false, values);
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


    private static void assertNumbersHaveSameType(Number[] values) {
        Class<? extends Number> numberClass = values[0].getClass();
        if (!Arrays.stream(values).allMatch(v -> numberClass == v.getClass())) {
            throw new IllegalArgumentException(NUMBERS_TYPE_ERR_MESSAGE);
        }
    }

}
