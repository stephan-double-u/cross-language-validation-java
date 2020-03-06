package de.swa.clv.constraints;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Arrays;
import java.util.List;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import static de.swa.clv.json.JsonUtil.asArray;
import static de.swa.clv.json.JsonUtil.asKey;
import static de.swa.clv.json.JsonUtil.quoted;

public class RegEx extends ConstraintRoot {

    private static Logger log = LoggerFactory.getLogger(RegEx.class);

    private static final String type = "REGEX_ANY";

    private final String messageDefault = "{validation.constraint.regex}";

    private final List<Pattern> patterns;

    private RegEx(String... regex) {
        super();
        setObjectValues(Arrays.asList((Object[]) regex));
        patterns = Arrays.asList(regex).stream()
                .filter(r -> r != null)
                .map(r -> Pattern.compile(r))
                .collect(Collectors.toList());
    }

    /**
     * The element that should be validated must match one of the given regular expressions.
     * <p/>
     * Supported types are:
     * <ul>
     * <li>{@code String} (string is evaluated)</li>
     * <li>{@code Enum<?>} (name of the enumeration is evaluated)</li>
     * </ul>
     * <p/>
     *
     * @param regex the regular expressions one of which must match the element
     */
    public static RegEx any(final String... regex) {
        if (Arrays.asList(regex).contains(null)) {
            throw new IllegalArgumentException("Null values are not allowed");
        }
        final RegEx constraint = new RegEx(regex);
        return constraint;
    }

    @Override
    public boolean isSupportedType(final Class<?> clazz) {
        return String.class.isAssignableFrom(clazz)
                || Enum.class.isAssignableFrom(clazz);
    }

    @Override
    public boolean validate(final Object object, final Object constraintObjectIgnored) {
        if (object == null) {
            return true;
        }
        if (!isSupportedType(object.getClass())) {
            throw new IllegalArgumentException("Unsupported type: " + object.getClass());
        }
        return patterns.stream()
                .peek(pattern -> log.debug("'" + object.toString() + (pattern.matcher(object.toString()).find() ? "' does" : "' does NOT") +" match regex '" + pattern + "'"))
                .map(pattern -> pattern.matcher(object.toString()).find())
                .filter(found -> found == true)
                .findFirst().orElse(false);
    }

    @Override
    public String serializeToJson() {
        return asKey("type") + quoted(type) + "," + asKey("values") + asArray(getValues());
    }

    @Override
    public String getType() {
        return type;
    }
}
