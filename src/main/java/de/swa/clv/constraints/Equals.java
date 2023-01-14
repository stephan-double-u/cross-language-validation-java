package de.swa.clv.constraints;

import de.swa.clv.util.TypeHelper;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.*;

import static de.swa.clv.json.JsonUtil.*;

public abstract class Equals extends Constraint {

    Equals() {
    }

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

    public static EqualsNoneRef noneRefNorNull(final String... values) {
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

}
