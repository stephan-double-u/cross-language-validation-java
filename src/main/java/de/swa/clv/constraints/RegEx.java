package de.swa.clv.constraints;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import static de.swa.clv.json.JsonUtil.asArray;
import static de.swa.clv.json.JsonUtil.asKey;
import static de.swa.clv.json.JsonUtil.quoted;

public class RegEx extends ConstraintRoot {

    private static final Logger log = LoggerFactory.getLogger(RegEx.class);

    private static final String TYPE = "REGEX_ANY";

    private final List<Pattern> patterns;

    private RegEx(String... regex) {
        super();
        setObjectValues(Arrays.asList(regex));
        patterns = Arrays.stream(regex)
                .filter(Objects::nonNull)
                .map(Pattern::compile)
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
        return new RegEx(regex);
    }

    @Override
    public boolean isSupportedType(final Class<?> clazz) {
        return String.class.isAssignableFrom(clazz)
                || Enum.class.isAssignableFrom(clazz)
                || Number.class.isAssignableFrom(clazz);
    }

    @Override
    public boolean validate(final Object object, final Object constraintObjectIgnored) {
        if (object == null) {
            return false;
        }
        // TODO? is resp. should be already checked in ValidationRules.validateConstraint, therefore obsolete here
        if (!isSupportedType(object.getClass())) {
            throw new IllegalArgumentException("Unsupported type: " + object.getClass());
        }
        return patterns.stream()
                .peek(pattern -> log.debug("'" + object.toString() + (pattern.matcher(object.toString()).find()
                        ? "' does" : "' does NOT") +" match regex '" + pattern + "'"))
                .map(pattern -> pattern.matcher(object.toString()).find())
                .filter(found -> found)
                .findFirst().orElse(false);
    }

    @Override
    public String serializeToJson() {
        return asKey("type") + quoted(TYPE) + "," + asKey("values") + asArray(getValues());
    }

    @Override
    public String getType() {
        return TYPE;
    }
}
