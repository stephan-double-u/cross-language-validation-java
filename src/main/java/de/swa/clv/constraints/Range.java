package de.swa.clv.constraints;

import de.swa.clv.util.TypeHelper;

import java.util.Arrays;

import static de.swa.clv.json.JsonUtil.asKey;
import static de.swa.clv.json.JsonUtil.quoted;

public class Range extends ConstraintRoot {

    private static final String TYPE = "RANGE";

    public static final long SAVE_INTEGER_MAX = (1L << 53) -1;
    public static final long SAVE_INTEGER_MIN = -((1L << 53) - 1);

    private final String messageDefault = "{validation.constraint.range}";

    private Range() {
    }

    /**
     * The value of the property that should be validated against this constraint must be >= {@code minValue}.
     * <p/>
     * Supported types are:
     * <ul>
     * <li>&lt;T extends Number & Comparable&gt;</li>
     * </ul>
     * <p/>
     *
     * @param minValue the minimal value of the element
     * @param <T> T extends Number & Comparable
     */
    public static <T extends Number & Comparable> Range min(final T minValue) {
        if (minValue == null) {
            throw new IllegalArgumentException("Null values are not allowed");
        }
        final Range constraint = new Range();
        constraint.setObjectValues(Arrays.asList(minValue, null));
        constraint.validateValuesOrFail(null);
        return constraint;
    }

    /**
     * The value of the property that should be validated against this constraint must be <= {@code maxValue}.
     * <p/>
     * Supported types are:
     * <ul>
     * <li>{@code Number & Comparable} ...</li>
     * </ul>
     * <p/>
     *
     * @param maxValue the maximal value of the element
     * @param <T> T extends Number & Comparable
     */
    public static <T extends Number & Comparable> Range max(final T maxValue) {
        if (maxValue == null) {
            throw new IllegalArgumentException("Null values are not allowed");
        }
        final Range constraint = new Range();
        constraint.setObjectValues(Arrays.asList(null, maxValue));
        constraint.validateValuesOrFail(null);
        return constraint;
    }

    /**
     * The value of the property that should be validated against this constraint must between {@code minValue} and
     * {@code maxValue}.
     * <p/>
     * Supported types are:
     * <ul>
     * <li>{@code Number & Comparable} ...</li>
     * </ul>
     * <p/>
     *
     * @param minValue the minimal value of the element
     * @param maxValue the maximal value of the element
     * @param <T> T extends Number & Comparable
     */
    public static <T extends Number & Comparable> Range minMax(final T minValue, final T maxValue) {
        if (minValue == null || maxValue == null) {
            throw new IllegalArgumentException("Null values are not allowed");
        }
        final Range constraint = new Range();
        constraint.setObjectValues(Arrays.asList(minValue, maxValue));
        constraint.validateValuesOrFail(null);
        return constraint;
    }

    @Override
    public boolean isSupportedType(final Class<?> clazz) {
        final Class<?> wrappedClass = (clazz.isPrimitive()) ? TypeHelper.PRIMITIVE_TO_WRAPPER_TYPES.get(clazz) : clazz;
        boolean classIsComarableNumber = Number.class.isAssignableFrom(wrappedClass)
                && Comparable.class.isAssignableFrom(wrappedClass);
        boolean classAndValuesHaveSameType = true;
        final Object min = getValues().get(0);
        final Object max = getValues().get(1);
        if (min != null) {
            classAndValuesHaveSameType &= min.getClass().equals(wrappedClass);
        }
        if (max != null) {
            classAndValuesHaveSameType &= max.getClass().equals(wrappedClass);
        }
        return classIsComarableNumber && classAndValuesHaveSameType;
    }

    @Override
    public boolean validateValuesOrFail(final Class<?> ignore) {
        if (getValues().size() != 2) {
            throw new IllegalArgumentException("Range needs min and max values");
        }
        final Object min = getValues().get(0);
        final Object max = getValues().get(1);
        if (min != null && max != null
                && ((Comparable) min).compareTo(max) > 0) {
            throw new IllegalArgumentException("Range min/max values must be min <= max");
        }
        return true;
    }

    @Override
    public boolean validate(final Object object, final Object notRelevant) {
        if (object == null) {
            return false;
        }
        final Object min = getValues().get(0);
        final Object max = getValues().get(1);
        boolean match = true;
        if (min != null) {
            match &= ((Comparable) min).compareTo(object) <= 0;
        }
        if (max != null) {
            match &= ((Comparable) max).compareTo(object) >= 0;
        }
        return match;
    }

    @Override
    public String serializeToJson() {
        final Object min = getValues().get(0);
        final Object max = getValues().get(1);
        final String minJson = min != null ? asKey("min") + min : "";
        final String maxJson = max != null ? asKey("max") + max : "";
        final String delimiter = ("".equals(minJson) || "".equals(maxJson)) ? "" : ",";
        return asKey("type") + quoted(TYPE) + "," + minJson + delimiter + maxJson;
    }

    @Override
    public String getType() {
        return TYPE;
    }
}
