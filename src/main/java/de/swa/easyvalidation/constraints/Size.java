package de.swa.easyvalidation.constraints;

import de.swa.easyvalidation.json.JsonUtil;

import java.lang.reflect.Array;
import java.util.Arrays;
import java.util.Collection;
import java.util.Map;

public class Size extends Constraint {

    private static final String type = "SIZE";
    private final String messageDefault = "{validation.constraint.size}";

    /**
     * The size of the element that should be validated against this constraint must be greater than the specified value
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
        constraint.setNumberValues(Arrays.asList((Number) Integer.valueOf(minSize), null));
        return constraint;
    }

    /**
     * The size of the element that should be validated against this constraint must be less than the specified value
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
     * @param maxSize
     *            the minimal size of the element
     * 
     * @author Stephan Wack
     */
    public static Size max(final int maxSize) {
        final Size constraint = new Size();
        constraint.setNumberValues(Arrays.asList(null, (Number) Integer.valueOf(maxSize)));
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
        constraint.setNumberValues(Arrays.asList((Number) Integer.valueOf(minSize), (Number) Integer.valueOf(maxSize)));
        return constraint;
    }

    @Override
    public boolean isSupportedType(final Class<?> clazz) {
        return CharSequence.class.isAssignableFrom(clazz) 
                || Collection.class.isAssignableFrom(clazz)
                || Map.class.isAssignableFrom(clazz) 
                || clazz.isArray();
    }

    @Override
    public boolean validateArgumentsOrFail(final Class<?> ignore) {
        final Integer min = (Integer) getValues().get(0);
        final Integer max = (Integer) getValues().get(1);
        if (min != null && min < 0 || max != null && max < 0) {
            throw new IllegalArgumentException("Size min/max values must be >= 0");
        }
        return true;
    }
    
    @Override
    public boolean validate(final Object object, final Object contraintObject) {
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
        return JsonUtil.asObject(JsonUtil.asKey("type") + JsonUtil.quoted(type) + "," + JsonUtil.asKey("min") + minValue + "," + JsonUtil.asKey("max") + maxValue);
    }

}
