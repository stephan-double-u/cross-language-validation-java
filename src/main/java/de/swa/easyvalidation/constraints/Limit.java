package de.swa.easyvalidation.constraints;

import static de.swa.easyvalidation.json.JsonUtil.asKey;
import static de.swa.easyvalidation.json.JsonUtil.quoted;

import java.util.Arrays;
import java.util.Comparator;

import de.swa.easyvalidation.util.TypeHelper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class Limit extends Constraint {

    private static Logger log = LoggerFactory.getLogger(Limit.class);

    private static final String type = "LIMIT";
    private String messageDefault = "{validation.constraint.limit}";

    public static long SAVE_INTEGER_MAX = (1L << 53) -1;
    public static long SAVE_INTEGER_MIN = -((1L << 53) - 1);

    /**
     * The lessThan of the element that should be validated against this constraint must be not less than the specified
     * lessThan.
     * <p/>
     * Supported types are:
     * <ul>
     * <li>{@code Number} that has a meaningful longValue() ...</li>
     * </ul>
     * <p/>
     *
     * @param minValue
     *            the minimal lessThan of the element
     * 
     * @author Stephan Wack
     */
    public static Limit min(final long minValue) {
        final Limit constraint = new Limit();
        constraint.setObjectValues(Arrays.asList((Number) Long.valueOf(minValue), null));
        return constraint;
    }

    public static Limit max(final long maxValue) {
        final Limit constraint = new Limit();
        constraint.setObjectValues(Arrays.asList(null, (Number) Long.valueOf(maxValue)));
        return constraint;
    }

    public static Limit minMax(final long minValue, final long maxValue) {
        final Limit constraint = new Limit();
        constraint.setObjectValues(Arrays.asList((Number) Long.valueOf(minValue), (Number) Long.valueOf(maxValue)));
        return constraint;
    }

    @Override
    public boolean isSupportedType(Class<?> clazz) {
        if (clazz.isPrimitive()) {
            clazz = TypeHelper.PRIMITIVE_TO_WRAPPER_TYPES.get(clazz);
        }
        return Number.class.isAssignableFrom(clazz);
    }

    @Override
    public boolean validate(final Object object, final Object constraintObject) {
        if (object == null) {
            return true;
        }
        final Long minValue = (Long) getValues().get(0);
        final Long maxValue = (Long) getValues().get(1);
        // TODO Number is not o.k. ...
        if (object instanceof Number) {
            final Long longValue = ((Number) object).longValue();
            return (minValue == null || minValue.compareTo(longValue) <= 0) 
                    && (maxValue == null || maxValue.compareTo(longValue) >= 0);
        } else {
            throw new IllegalArgumentException("Unsupported type: " + object.getClass());
        }
    }

    // TODO not used (yet) ...
    private static final LongComparator<Long> numberComparator = new LongComparator<>();
    static class LongComparator<T extends Number & Comparable<T>> implements Comparator<T> {
        @Override
        public int compare(final T a, final T b) throws ClassCastException {
            return a.compareTo(b);
        }
    }    
    
    @Override
    public String serializeToJson() {
        Long minValue = (Long) getValues().get(0);
        Long maxValue = (Long) getValues().get(1);
        return asKey("type") + quoted(type) + "," + asKey("min") + minValue + "," + asKey("max") + maxValue;
    }

}
