package de.swa.clv.constraints;

import de.swa.clv.util.TypeHelper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.regex.Pattern;

import static de.swa.clv.json.JsonUtil.*;

public abstract class RegExConstraint extends Constraint {

    private static final Logger log = LoggerFactory.getLogger(RegExConstraint.class);

    private final List<Pattern> patterns;

    RegExConstraint(String... regex) {
        super();
        setValues(List.of(regex));
        patterns = Arrays.stream(regex)
                .filter(Objects::nonNull)
                .map(Pattern::compile)
                .toList();
    }

    @Override
    public boolean isSupportedType(final Class<?> clazz) {
        final Class<?> wrappedClass = (clazz.isPrimitive()) ? TypeHelper.PRIMITIVE_TO_WRAPPER_TYPES.get(clazz) : clazz;
        return String.class.isAssignableFrom(clazz)
                || Enum.class.isAssignableFrom(clazz)
                || Number.class.isAssignableFrom(wrappedClass);
    }

    @Override
    public boolean validate(final Object object, final Object constraintObjectIgnored) {
        if (object == null) {
            return false;
        }
        // TODO? This is resp. should be already checked in ValidationRules.validateConstraint, therefore obsolete here
        if (!isSupportedType(object.getClass())) {
            throw new IllegalArgumentException("Unsupported type: " + object.getClass());
        }
        boolean typeIsRegExAny = getToken().equals(RegExAny.TYPE);
        Boolean match = patterns.stream()
                .peek(pattern -> log.debug("'" + object + (pattern.matcher(object.toString()).find()
                        ? "' does" : "' does NOT") + " match regex '" + pattern + "'"))
                .map(pattern -> pattern.matcher(object.toString()).find())
                .filter(found -> found)
                .findFirst()
                .orElse(false);
        return typeIsRegExAny == match;
    }

    @Override
    @SuppressWarnings({"squid:S5361"})
    public String serializeToJson() {
        List<String> regExWithDoubledBackslashes = getValues().stream()
                .map(regEx -> ((String) regEx).replaceAll("\\\\", "\\\\\\\\"))
                .toList();
        String nullEqualsToJson = getJsonForNullEqualsTrue(false);
        return asKey("type") + quoted(getToken()) + "," + asKey("values") + asArray((List) regExWithDoubledBackslashes)
                + nullEqualsToJson;
    }

}
