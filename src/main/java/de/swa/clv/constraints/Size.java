package de.swa.clv.constraints;

import java.lang.reflect.Array;
import java.util.Arrays;
import java.util.Collection;
import java.util.Map;

import static de.swa.clv.json.JsonUtil.asKey;
import static de.swa.clv.json.JsonUtil.quoted;

public class Size extends ConstraintRoot {

    private static final String TYPE = "SIZE";

    private Size() {
    }

    /**
     * The size of the element that should be validated against this constraint must be greater than the specified object
     * (included).
     * <p/>
     * Supported types are:
     * <ul>
     * <li>{@code CharSequence} (length of character sequence is evaluated)</li>
     * <li>{@code Collection} (collection size is evaluated)</li>
     * <li>{@code Map} (map size is evaluated)</li>
     * <li>Array (array length is evaluated)</li>
     * </ul>
     * <p/>
     *
     * @param minSize
     *            the minimal size of the element
     * 
     * @author Stephan Wack
     */
    public static Size min(final int minSize) {
        final Size constraint = new Size();
        constraint.setObjectValues(Arrays.asList(Integer.valueOf(minSize), null));
        constraint.validateValuesOrFail(null, null);
        return constraint;
    }

    /**
     * The size of the element that should be validated against this constraint must be less than the specified object
     * (included).
     * <p/>
     * Supported types are:
     * <ul>
     * <li>{@code String} (string length is evaluated)</li>
     * <li>{@code Collection} (collection size is evaluated)</li>
     * <li>{@code Map} (map size is evaluated)</li>
     * <li>Array (array length is evaluated)</li>
     * </ul>
     * <p/>
     *
     * @param maxSize
     *            the minimal size of the element
     * 
     * @author Stephan Wack
     */
    public static Size max(final int maxSize) {
        final Size constraint = new Size();
        constraint.setObjectValues(Arrays.asList(null, Integer.valueOf(maxSize)));
        constraint.validateValuesOrFail(null, null);
        return constraint;
    }

    /**
     * The size of the element that should be validated against this constraint must be between the specified boundaries
     * (included).
     * <p/>
     * Supported types are:
     * <ul>
     * <li>{@code CharSequence} (length of character sequence is evaluated)</li>
     * <li>{@code Collection} (collection size is evaluated)</li>
     * <li>{@code Map} (map size is evaluated)</li>
     * <li>Array (array length is evaluated)</li>
     * </ul>
     * <p/>
     *
     * @param minSize
     *            the minimal size of the element
     * @param maxSize
     *            the minimal size of the element
     * 
     * @author Stephan Wack
     */
    public static Size minMax(final int minSize, final int maxSize) {
        final Size constraint = new Size();
        constraint.setObjectValues(Arrays.asList(Integer.valueOf(minSize), Integer.valueOf(maxSize)));
        constraint.validateValuesOrFail(null, null);
        return constraint;
    }

    @Override
    public boolean isSupportedType(final Class<?> clazz) {
        return String.class == clazz // String is final
                || Collection.class.isAssignableFrom(clazz)
                || Map.class.isAssignableFrom(clazz) 
                || clazz.isArray();
    }

    @Override
    public void validateValuesOrFail(final Class<?> ignore, final Class<?> ignoreToo) {
        if (getValues().size() != 2) {
            throw new IllegalArgumentException("Size has neither min nor max value");
        }
        final Integer min = (Integer) getValues().get(0);
        final Integer max = (Integer) getValues().get(1);
        if (min != null && min < 0 || max != null && max < 0 || min != null && max != null && min > max) {
            throw new IllegalArgumentException("Size min/max values must be >= 0 and min < max");
        }
    }
    
    @Override
    public boolean
    validate(final Object object, final Object ignored) {
        if (object == null) {
            return true;
        }
        final Integer minValue = (Integer) getValues().get(0);
        final Integer maxValue = (Integer) getValues().get(1);
        final int objectLength;
        if (object instanceof CharSequence) {
            objectLength = ((CharSequence) object).length();
        } else if (object instanceof Collection) {
            objectLength = ((Collection<?>) object).size();
        } else if (object instanceof Map) {
            objectLength = ((Map<?, ?>) object).size();
        } else if (object.getClass().isArray()) {
            objectLength = Array.getLength(object);
        } else {
            throw new IllegalArgumentException("Unsupported type: " + object.getClass());
        }
        return (minValue == null || objectLength >= minValue) && (maxValue == null || objectLength <= maxValue);
    }

    @Override
    public String serializeToJson() {
        final Integer minValue = (Integer) getValues().get(0);
        final Integer maxValue = (Integer) getValues().get(1);
        final String minJson = minValue != null ? asKey("min") + minValue : "";
        final String maxJson = maxValue != null ? asKey("max") + maxValue : "";
        final String delimiter = ("".equals(minJson) || "".equals(maxJson)) ? "" : ",";
        return asKey("type") + quoted(TYPE) + "," + minJson + delimiter + maxJson;
    }

    @Override
    public String getType() {
        return TYPE;
    }
}
