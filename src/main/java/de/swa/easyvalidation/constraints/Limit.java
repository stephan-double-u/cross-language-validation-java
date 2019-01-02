package de.swa.easyvalidation.constraints;

import static de.swa.easyvalidation.json.JsonUtil.asKey;
import static de.swa.easyvalidation.json.JsonUtil.asObject;
import static de.swa.easyvalidation.json.JsonUtil.quoted;

import java.util.Arrays;
import java.util.Collection;
import java.util.Comparator;
import java.util.Map;

import de.swa.easyvalidation.util.TypeHelper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class Limit extends Constraint {

    private static Logger log = LoggerFactory.getLogger(Limit.class);

    private static final String type = "LIMIT";
    private String messageDefault = "{validation.constraints.limit}";

    /**
     * The value of the element that should be validated against this constraint must be not less than the specified
     * value.
     * <p/>
     * Supported types are:
     * <ul>
     * <li>{@code Number} that habe a meaningful longValue() ...</li>
     * </ul>
     * <p/>
     *
     * @param minValue
     *            the minimal value of the element
     * 
     * @author Stephan Wack
     */
    public static Limit min(long minValue) {
        Limit constraint = new Limit();
        constraint.setNumberValues(Arrays.asList((Number) Long.valueOf(minValue), null));
        return constraint;
    }

    public static Limit max(long maxValue) {
        Limit constraint = new Limit();
        constraint.setNumberValues(Arrays.asList(null, (Number) Long.valueOf(maxValue)));
        return constraint;
    }

    public static Limit minMax(long minValue, long maxValue) {
        Limit constraint = new Limit();
        constraint.setNumberValues(Arrays.asList((Number) Long.valueOf(minValue), (Number) Long.valueOf(maxValue)));
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
    public boolean validate(Object object, Object contraintObject) {
        if (object == null) {
            return true;
        }
        Long minValue = (Long) getValues().get(0);
        Long maxValue = (Long) getValues().get(1);
        // TODO Number is not o.k. ...
        if (object instanceof Number) {
            Long longValue = ((Number) object).longValue();
            return (minValue == null || minValue.compareTo(longValue) <= 0) 
                    && (maxValue == null || maxValue.compareTo(longValue) >= 0);
        } else {
            throw new IllegalArgumentException("Unsupported type: " + object.getClass());
        }
    }

    // not used ...
    private static LongComparator<Long> numberComparator = new LongComparator<>();
    static class LongComparator<T extends Number & Comparable<T>> implements Comparator<T> {
        public int compare(T a, T b) throws ClassCastException {
            return a.compareTo(b);
        }
    }    
    
    @Override
    public String serializeToJson() {
        Long minValue = (Long) getValues().get(0);
        Long maxValue = (Long) getValues().get(1);
        return asObject(asKey("type") + quoted(type) + "," + asKey("min") + minValue + "," + asKey("max") + maxValue);
    }

    // TODO -> JUnit test
    public static void main(String[] args) {
        Limit limit = Limit.minMax(123, 1234567890);
        log.debug(limit.serializeToJson());

        log.debug("" + limit.validate(12345, null));
        log.debug("" + limit.validate(0, null));
        log.debug("" + limit.validate(Long.MAX_VALUE, null));
    }
}
