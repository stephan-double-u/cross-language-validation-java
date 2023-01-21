package de.swa.clv.constraints;

import de.swa.clv.util.TypeHelper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.Arrays;

import static de.swa.clv.json.JsonUtil.*;

public class Range extends Constraint implements IsCreateConstraint, IsUpdateConstraint {

    private static final Logger log = LoggerFactory.getLogger(Range.class);

    private static final String TYPE = "RANGE";

    /**
     * The constant represents the maximum safe integer in JavaScript, i.e. 2^53 – 1.
     */
    public static final long MAX_SAVE_INTEGER_JAVASCRIPT = (1L << 53) -1;
    /**
     * The constant represents the minimum safe integer in JavaScript, i.e. -(2^53 - 1).
     */
    public static final long MIN_SAVE_INTEGER_JAVASCRIPT = -((1L << 53) - 1);

    private Range(boolean nullEqualsTrue, Object minValue, Object maxValue) {
        setNullEqualsTrue(nullEqualsTrue);
        setValues(Arrays.asList(minValue, maxValue));
        validateValuesOrFail(null, null);
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
        return new Range(false, minValue, null);
    }

    public static <T extends Number & Comparable<T>> Range minOrNull(final T minValue) {
        validateNotNull(minValue, minValue);
        return new Range(true, minValue, null);
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
        return new Range(false, null, maxValue);
    }

    public static <T extends Number & Comparable<T>> Range maxOrNull(final T maxValue) {
        validateNotNull(maxValue, maxValue);
        return new Range(true, null, maxValue);
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
        return new Range(false, minValue, maxValue);
    }

    public static <T extends Number & Comparable<T>> Range minMaxOrNull(final T minValue, final T maxValue) {
        validateNotNull(minValue, maxValue);
        return new Range(true, minValue, maxValue);
    }

    public static Range min(final LocalDate minValue) {
        validateNotNull(minValue, minValue);
        return new Range(false, minValue, null);
    }

    public static Range minOrNull(final LocalDate minValue) {
        validateNotNull(minValue, minValue);
        return new Range(true, minValue, null);
    }

    public static Range max(final LocalDate maxValue) {
        validateNotNull(maxValue, maxValue);
        return new Range(false, null, maxValue);
    }

    public static Range maxOrNull(final LocalDate maxValue) {
        validateNotNull(maxValue, maxValue);
        return new Range(true, null, maxValue);
    }

    public static Range minMax(final LocalDate minValue, final LocalDate maxValue) {
        validateNotNull(minValue, maxValue);
        return new Range(false, minValue, maxValue);
    }

    public static Range minMaxOrNull(final LocalDate minValue, final LocalDate maxValue) {
        validateNotNull(minValue, maxValue);
        return new Range(true, minValue, maxValue);
    }

    public static Range min(final LocalDateTime minValue) {
        validateNotNull(minValue, minValue);
        return new Range(false, minValue, null);
    }

    public static Range minOrNull(final LocalDateTime minValue) {
        validateNotNull(minValue, minValue);
        return new Range(true, minValue, null);
    }

    public static Range max(final LocalDateTime maxValue) {
        validateNotNull(maxValue, maxValue);
        return new Range(false, null, maxValue);
    }

    public static Range maxOrNull(final LocalDateTime maxValue) {
        validateNotNull(maxValue, maxValue);
        return new Range(true, null, maxValue);
    }

    public static Range minMax(final LocalDateTime minValue, final LocalDateTime maxValue) {
        validateNotNull(minValue, maxValue);
        return new Range(false, minValue, maxValue);
    }

    public static Range minMaxOrNull(final LocalDateTime minValue, final LocalDateTime maxValue) {
        validateNotNull(minValue, maxValue);
        return new Range(true, minValue, maxValue);
    }

    private static void validateNotNull(Object minValue, Object maxValue) {
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
    public boolean validate(final Object objectToValidate, final Object notRelevant) {
        if (objectToValidate == null) {
            log.debug("'Null object equals to {}", doesNullEqualsTrue());
            return doesNullEqualsTrue();
        }
        if (Number.class.isAssignableFrom(objectToValidate.getClass())) {
            return compareAsBigDecimal(objectToValidate);
        } else {
            return compareAsComparable(objectToValidate);
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
        Object min = getValues().get(0);
        Object max = getValues().get(1);
        String minJson = min != null ? asKey("min") + quoteIfNeeded(min) : "";
        String maxJson = max != null ? asKey("max") + quoteIfNeeded(max) : "";
        String delimiter = ("".equals(minJson) || "".equals(maxJson)) ? "" : ",";
        String nullEqualsToJson = getJsonForNullEqualsTrue(false);
        return asKey("type") + quoted(TYPE) + "," + minJson + delimiter + maxJson + nullEqualsToJson;
    }

    @Override
    public String getToken() {
        return TYPE;
    }
}
