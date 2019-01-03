package de.swa.easyvalidation.constraints;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import static de.swa.easyvalidation.json.JsonUtil.*;

import java.util.Arrays;
import java.util.regex.Matcher;

public class RegExp extends Constraint {

    private static Logger log = LoggerFactory.getLogger(RegExp.class);

    private static final String type = "REGEXP";
    
    private String messageDefault = "{validation.constraints.regexp}";

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
    public static RegExp any(String... regexp) {
        RegExp constraint = new RegExp();
        constraint.setStringValues(Arrays.asList(regexp));
        return constraint;
    }

    @Override
    public boolean isSupportedType(Class<?> clazz) {
        return CharSequence.class.isAssignableFrom(clazz)
                || Enum.class.isAssignableFrom(clazz);
    }

    @Override
    public boolean validateArgumentsOrFail(Class<?> ignore) {
        if (getValues().contains(null)) {
            throw new IllegalArgumentException("RegExp regexp must not be null");
        }
        return true;
    }
    
    @Override
    public boolean validate(Object object, Object contraintObjectIgnored) {
        if (object == null) {
            return true;
        }
        //TODO use cached matcher ...
        // One regexp must match
        for (Object value : getValues()) {
            String regexp = (String) value;
            java.util.regex.Pattern pattern = java.util.regex.Pattern.compile(regexp);
            Matcher matcher = pattern.matcher(object.toString());
            boolean match = matcher.matches();
            log.debug("'" + object.toString() + (match ? "' does" : "' does NOT") +" match regexp '" + regexp + "'");
            if (match) {
                return true;
            };
        }
        return false;
    }

    @Override
    public String serializeToJson() {
        return asObject(asKey("type") + quoted(type) + "," + asKey("values") + asArray(getValues()));
    }

}
