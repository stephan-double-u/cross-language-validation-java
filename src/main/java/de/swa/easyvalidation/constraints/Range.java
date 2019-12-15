package de.swa.easyvalidation.constraints;

import java.util.Arrays;

/**
 * Provides static methods to create all different kinds of range constraints.
 */
public class Range {

    /**
     * The value of the property that should be validated against this constraint must be >= {@code value}.
     * <p/>
     * Supported types are:
     * <ul>
     * <li>{@code Number & Comparable} ...</li>
     * </ul>
     * <p/>
     *
     * @param value
     *            the minimal value of the element
     * @param <T> T extends Number & Comparable
     */
    public static <T extends Number & Comparable> RangeAny min(final T value) {
        if (value == null) {
            throw new IllegalArgumentException("Null values are not allowed");
        }
        RangeAny rangeAny = new RangeAny().min(value);
        return rangeAny;
    }

    /**
     *
     * @param maxValue
     * @param <T> T extends Number & Comparable
     * @return
     */
    public static <T extends Number & Comparable> RangeAny max(final T maxValue) {
        if (maxValue == null) {
            throw new IllegalArgumentException("Null values are not allowed");
        }
        RangeAny rangeAny = new RangeAny().max(maxValue);
        return rangeAny;
    }

    public static <T extends Number & Comparable> RangeAny minMax(final T minValue, final T maxValue) {
        if (minValue == null || maxValue == null) {
            throw new IllegalArgumentException("Null values are not allowed");
        }
        RangeAny rangeAny = new RangeAny().min(minValue).max(maxValue);
        return rangeAny;
    }

    public static <T extends Number & Comparable> RangeAny minAny(final T... minValues) {
        if (Arrays.asList(minValues).contains(null)) {
            throw new IllegalArgumentException("Null values are not allowed");
        }
        RangeAny rangeAny = new RangeAny().minAny(minValues);
        return rangeAny;
    }

    public static <T extends Number & Comparable> RangeAny maxAny(final T... maxValues) {
        if (Arrays.asList(maxValues).contains(null)) {
            throw new IllegalArgumentException("Null values are not allowed");
        }
        RangeAny rangeAny = new RangeAny().maxAny(maxValues);
        return rangeAny;
    }


    /**
     * The value of the property that should be validated against this constraint must be >= {@code minValue}.
     * The default comparison algorithm is {@code ComparisonType.LEXICOGRAPHICAL_UNICODE}.
     * <p/>
     * Supported property type is:
     * <ul>
     * <li>{@code String}</li>
     * </ul>
     * <p/>
     *
     * @param minValue the value to compare with
     * @return a {@code RangeAny} constraint
     */
    public static RangeStringAny min(final String minValue) {
        if (minValue == null) {
            throw new IllegalArgumentException("Null values are not allowed");
        }
        RangeStringAny rangeAny = new RangeStringAny().min(minValue);
        return rangeAny;
    }

    /**
     * The value of the property that should be validated against this constraint must be <= {@code maxValue}.
     * The default comparison algorithm is {@code ComparisonType.LEXICOGRAPHICAL_UNICODE}.
     * <p/>
     * Supported property type is:
     * <ul>
     * <li>{@code String}</li>
     * </ul>
     * <p/>
     *
     * @param maxValue the value to compare with
     * @return a {@code RangeAny} constraint
     */
    public static RangeStringAny max(final String maxValue) {
        if (maxValue == null) {
            throw new IllegalArgumentException("Null values are not allowed");
        }
        RangeStringAny rangeAny = new RangeStringAny().max(maxValue);
        return rangeAny;
    }

    /**
     * The value of the property that should be validated against this constraint must be >= {@code minValue} and <= {@code maxValue}.
     * The default comparison algorithm is {@code ComparisonType.LEXICOGRAPHICAL_UNICODE}.
     * <p/>
     * Supported property type is:
     * <ul>
     * <li>{@code String}</li>
     * </ul>
     * <p/>
     *
     * @param minValue the minimal value to compare with
     * @param maxValue the maximal value to compare with
     * @return a {@code RangeAny} constraint
     */
    public static RangeStringAny minMax(final String minValue, final String maxValue) {
        if (minValue == null || maxValue == null) {
            throw new IllegalArgumentException("Null values are not allowed");
        }
        RangeStringAny rangeAny = new RangeStringAny().min(minValue).max(maxValue);
        return rangeAny;
    }

    public static RangeStringAny minAny(final String... minValues) {
        if (Arrays.asList(minValues).contains(null)) {
            throw new IllegalArgumentException("Null values are not allowed");
        }
        RangeStringAny rangeAny = new RangeStringAny().minAny(minValues);
        return rangeAny;
    }

    public static RangeStringAny maxAny(final String... maxValues) {
        if (Arrays.asList(maxValues).contains(null)) {
            throw new IllegalArgumentException("Null values are not allowed");
        }
        RangeStringAny rangeAny = new RangeStringAny().maxAny(maxValues);
        return rangeAny;
    }
}
