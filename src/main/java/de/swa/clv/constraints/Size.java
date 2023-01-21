package de.swa.clv.constraints;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.lang.reflect.Array;
import java.util.Arrays;
import java.util.Collection;
import java.util.Map;

import static de.swa.clv.json.JsonUtil.asKey;
import static de.swa.clv.json.JsonUtil.quoted;

public class Size extends Constraint implements IsCreateConstraint, IsUpdateConstraint {

    private static final Logger log = LoggerFactory.getLogger(Size.class);

    private static final String TYPE = "SIZE";

    private Size(boolean nullEqualsTrue, Integer minSize, Integer maxSize) {
        setNullEqualsTrue(nullEqualsTrue);
        setValues(Arrays.asList(minSize, maxSize));
        validateValuesOrFail(null, null);
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
        return new Size(false, minSize, null);
    }

    public static Size minOrNull(final int minSize) {
        return new Size(true, minSize, null);
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
        return new Size(false, null, maxSize);
    }

    public static Size maxOrNull(final int maxSize) {
        return new Size(true, null, maxSize);
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
        return new Size(false, minSize, maxSize);
    }

    public static Size minMaxOrNull(final int minSize, final int maxSize) {
        return new Size(true, minSize, maxSize);
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
            throw new IllegalArgumentException("Size min/max values must be >= 0 and min <= max");
        }
    }
    
    @Override
    public boolean validate(final Object objectToValidate, final Object ignored) {
        if (objectToValidate == null) {
            log.debug("'Null object equals to {}", doesNullEqualsTrue());
            return doesNullEqualsTrue();
        }
        final Integer minValue = (Integer) getValues().get(0);
        final Integer maxValue = (Integer) getValues().get(1);
        final int objectLength;
        if (objectToValidate instanceof CharSequence charSequence) {
            objectLength = charSequence.length();
        } else if (objectToValidate instanceof Collection<?> collection) {
            objectLength = collection.size();
        } else if (objectToValidate instanceof Map<?, ?> map) {
            objectLength = map.size();
        } else if (objectToValidate.getClass().isArray()) {
            objectLength = Array.getLength(objectToValidate);
        } else {
            throw new IllegalArgumentException("Unsupported type: " + objectToValidate.getClass());
        }
        return (minValue == null || objectLength >= minValue) && (maxValue == null || objectLength <= maxValue);
    }

    @Override
    public String serializeToJson() {
        Integer minValue = (Integer) getValues().get(0);
        Integer maxValue = (Integer) getValues().get(1);
        String minJson = minValue != null ? asKey("min") + minValue : "";
        String maxJson = maxValue != null ? asKey("max") + maxValue : "";
        String delimiter = ("".equals(minJson) || "".equals(maxJson)) ? "" : ",";
        String nullEqualsToJson = getJsonForNullEqualsTrue(false);
        return asKey("type") + quoted(TYPE) + "," + minJson + delimiter + maxJson + nullEqualsToJson;
    }

    @Override
    public String getToken() {
        return TYPE;
    }
}
