package de.swa.clv.constraints;

import de.swa.clv.util.TypeHelper;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.Arrays;

import static de.swa.clv.json.JsonUtil.asKey;
import static de.swa.clv.json.JsonUtil.quoted;

public class Range extends Constraint {

    private static final String TYPE = "RANGE";

    public static final long SAVE_INTEGER_MAX = (1L << 53) -1;
    public static final long SAVE_INTEGER_MIN = -((1L << 53) - 1);

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
    public static <T extends Number & Comparable<T>> Range min(final T minValue) {
        validateNotNull(minValue, minValue);
        return newRange(minValue, null);
    }

    /**
     * The value of the property that should be validated against this constraint must be &lt;= {@code maxValue}.
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
    public static <T extends Number & Comparable<T>> Range max(final T maxValue) {
        validateNotNull(maxValue, maxValue);
        return newRange(null, maxValue);
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
    public static <T extends Number & Comparable<T>> Range minMax(final T minValue, final T maxValue) {
        validateNotNull(minValue, maxValue);
        return newRange(minValue, maxValue);
    }

    public static Range min(final LocalDate minValue) {
        validateNotNull(minValue, minValue);
        return newRange(minValue, null);
    }

    public static Range max(final LocalDate maxValue) {
        validateNotNull(maxValue, maxValue);
        return newRange(null, maxValue);
    }

    public static Range minMax(final LocalDate minValue, final LocalDate maxValue) {
        validateNotNull(minValue, maxValue);
        return newRange(minValue, maxValue);
    }

    public static Range min(final LocalDateTime minValue) {
        validateNotNull(minValue, minValue);
        return newRange(minValue, null);
    }

    public static Range max(final LocalDateTime maxValue) {
        validateNotNull(maxValue, maxValue);
        return newRange(null, maxValue);
    }

    public static Range minMax(final LocalDateTime minValue, final LocalDateTime maxValue) {
        validateNotNull(minValue, maxValue);
        return newRange(minValue, maxValue);
    }

    private static <T extends Number & Comparable<T>> Range newRange(Object minValue, Object maxValue) {
        final Range constraint = new Range();
        constraint.setObjectValues(Arrays.asList(minValue, maxValue));
        constraint.validateValuesOrFail(null, null);
        return constraint;
    }

    private static <T extends Number & Comparable<T>> void validateNotNull(Object minValue, Object maxValue) {
        if (minValue == null || maxValue == null) {
            throw new IllegalArgumentException(NULL_VALUE_ERR_MESSAGE);
        }
    }

    @Override
    public boolean isSupportedType(final Class<?> clazz) {
        final Class<?> wrappedClass = (clazz.isPrimitive()) ? TypeHelper.PRIMITIVE_TO_WRAPPER_TYPES.get(clazz) : clazz;
        boolean classIsComparableNumber = Number.class.isAssignableFrom(wrappedClass)
                && Comparable.class.isAssignableFrom(wrappedClass);
        Class<?> minMaxType = null;
        final Object min = getValues().get(0);
        final Object max = getValues().get(1);
        if (min != null) {
            minMaxType = min.getClass();
        } else if (max != null) {
            minMaxType = max.getClass();
        }
        return classIsComparableNumber && Number.class.isAssignableFrom(minMaxType)
                || (LocalDate.class == clazz || LocalDateTime.class == clazz) && clazz == minMaxType;
    }

    @Override
    public void validateValuesOrFail(final Class<?> ignore, final Class<?> ignoreToo) {
        if (getValues().size() != 2) {
            throw new IllegalArgumentException("Range needs min and max values");
        }
        final Object min = getValues().get(0);
        final Object max = getValues().get(1);
        if (min != null && max != null
                && ((Comparable) min).compareTo(max) > 0) {
            throw new IllegalArgumentException("Range min/max values must be min <= max");
        }
    }

    @Override
    public boolean validate(final Object object, final Object notRelevant) {
        if (object == null) {
            return false;
        }
        if (Number.class.isAssignableFrom(object.getClass())) {
            return compareAsBigDecimal(object);
        } else {
            return compareAsComparable(object);
        }
    }

    private boolean compareAsBigDecimal(Object object) {
        BigDecimal objectBD = new BigDecimal(object.toString());
        final Object min = getValues().get(0);
        final Object max = getValues().get(1);
        boolean match = true;
        if (min != null) {
            BigDecimal minBD = new BigDecimal(min.toString());
            match = minBD.compareTo(objectBD) <= 0;
        }
        if (max != null) {
            BigDecimal maxBD = new BigDecimal(max.toString());
            match &= maxBD.compareTo(objectBD) >= 0;
        }
        return match;
    }

    private boolean compareAsComparable(final Object object) {
        final Object min = getValues().get(0);
        final Object max = getValues().get(1);
        boolean match = true;
        if (min != null) {
            match = ((Comparable) min).compareTo(object) <= 0;
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
        final String quote = (min instanceof LocalDate || max instanceof LocalDate) ? "\"" : "";
        final String minJson = min != null ? asKey("min") + quote + min + quote : "";
        final String maxJson = max != null ? asKey("max") + quote + max + quote : "";
        final String delimiter = ("".equals(minJson) || "".equals(maxJson)) ? "" : ",";
        return asKey("type") + quoted(TYPE) + "," + minJson + delimiter + maxJson;
    }

    @Override
    public String getType() {
        return TYPE;
    }
}
