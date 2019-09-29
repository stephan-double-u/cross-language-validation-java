package de.swa.easyvalidation.constraints;
import de.swa.easyvalidation.constraints.LessThanString.ComparisonType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import static de.swa.easyvalidation.json.JsonUtil.*;

import java.util.Arrays;

public class LessThan extends Constraint {

    private static Logger log = LoggerFactory.getLogger(LessThan.class);

    private static final String type = "LESS_THAN";

    /**
     * The lessThan of the element that should be validated against this constraint must be less than the specified lessThan.
     * The comparison algorithm used is {@code ComparisonType.LEXICOGRAPHICAL_UNICODE}.
     * <p/>
     * Supported types are:
     * <ul>
     * <li>{@code String} (the lessThan to compare with)</li>
     * </ul>
     * <p/>
     *
     * @param value the lessThan to compare
     * @return a {@code LessThanString} constraint
     */
    public static LessThanString lessThan(final String value) {
        return lessThan(value, ComparisonType.LEXICOGRAPHICAL_UNICODE);
    }

    /**
     * The value of the element that should be validated against this constraint must be less than the specified lessThan.
     * The comparison algorithm used is {@code ComparisonType.LEXICOGRAPHICAL_UNICODE}.
     * <p/>
     * Supported types are:
     * <ul>
     * <li>{@code String} (the lessThan to compare with)</li>
     * </ul>
     * <p/>
     *
     * @param value the lessThan to compare
     * @return a {@code LessThanString} constraint
     */
    public static LessThanString lessThan(final String value, ComparisonType comparisonType) {
        final LessThanString constraint = new LessThanString();
        constraint.setObjectValues(Arrays.asList(value, comparisonType.name()));
        return constraint;
    }

    @Override
    public boolean validate(final Object object, final Object ignored) {
        final String stringObject = (String) object;
        final String stringValue = (String) getValues().get(0);
        final String comparisonType = (String) getValues().get(1);
        boolean lessThan;
        if (ComparisonType.LEXICOGRAPHICAL_UNICODE.name().equals(comparisonType)) {
            lessThan = stringObject.compareTo(stringValue) < 0;
        } else if (ComparisonType.LEXICOGRAPHICAL_UNICODE_IGNORING_CASE.name().equals(comparisonType)) {
            lessThan = stringObject.toLowerCase().compareTo(stringValue.toLowerCase()) < 0;
        } else {
            throw new IllegalArgumentException("Should never happen! Unknown comparisonType: " + comparisonType);
        }
        log.debug(object + " is" + (lessThan ? " " : "NOT ") +"< than " + stringValue);
        return lessThan;
    }

    @Override
    public String serializeToJson() {
        return asKey("type") + quoted(type) + "," + asKey("lessThan") + asArray(getValues());
    }

    @Override
    public boolean isSupportedType(final Class<?> clazz) {
        return String.class == clazz;
    }

}
