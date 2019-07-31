package de.swa.easyvalidation.constraints;

import de.swa.easyvalidation.json.JsonUtil;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Arrays;
import java.util.regex.Matcher;

public class RegExp extends Constraint {

    private static Logger log = LoggerFactory.getLogger(Dates.class);

    private static final String type = "REGEXP";
    
    private final String messageDefault = "{validation.constraint.regexp}";

    /**
     * The element that should be validated must match one of the given regular expressions.
     * <p/>
     * Supported types are:
     * <ul>
     * <li>{@code CharSequence} (character sequence is evaluated)</li>
     * <li>{@code Enum<?>} (name of the enumeration is evaluated)</li>
     * </ul>
     * <p/>
     *
     * @param regexp the regular expression the element must match
     * 
     * @author Stephan Wack
     */
    public static RegExp any(final String... regexp) {
        final RegExp constraint = new RegExp();
        constraint.setStringValues(Arrays.asList(regexp));
        return constraint;
    }

    @Override
    public boolean isSupportedType(final Class<?> clazz) {
        return CharSequence.class.isAssignableFrom(clazz)
                || Enum.class.isAssignableFrom(clazz);
    }

    @Override
    public boolean validateArgumentsOrFail(final Class<?> ignore) {
        if (getValues().contains(null)) {
            throw new IllegalArgumentException("RegExp regexp must not be null");
        }
        return true;
    }
    
    @Override
    public boolean validate(final Object object, final Object contraintObjectIgnored) {
        if (object == null) {
            return true;
        }
        //TODO use cached matcher ...
        // One regexp must match
        for (final Object value : getValues()) {
            final String regexp = (String) value;
            final java.util.regex.Pattern pattern = java.util.regex.Pattern.compile(regexp);
            final Matcher matcher = pattern.matcher(object.toString());
            final boolean match = matcher.matches();
            log.debug("'" + object.toString() + (match ? "' does" : "' does NOT") +" match regexp '" + regexp + "'");
            if (match) {
                return true;
            };
        }
        return false;
    }

    @Override
    public String serializeToJson() {
        return JsonUtil.asKey("type") + JsonUtil.quoted(type) + "," + JsonUtil.asKey("values") + JsonUtil.asArray(getValues());
    }

}
