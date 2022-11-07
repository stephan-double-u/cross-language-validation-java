package de.swa.clv.constraints;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.time.LocalDateTime;

/**
 * Constraint to express expectations about the year of a date property.
 */
public class YearAny extends Year {

    private static final Logger log = LoggerFactory.getLogger(YearAny.class);

    public static final String TOKEN = "YEAR_ANY";

    YearAny(final boolean nullEqualsTrue, final Integer... years) {
        setNullEqualsTrue(nullEqualsTrue);
        setValues(getValuesAsObjectList(years));
    }

    @Override
    public String getToken() {
        return TOKEN;
    }

    @Override
    public void validateValuesOrFail(final Class<?> ignore, final Class<?> ignoreToo) {
        // validation already done in public API methods
    }

    @Override
    public boolean validate(final Object dateToValidate, final Object notRelevant) {
        if (dateToValidate == null) {
            log.debug("'Null object equals to {}", doesNullEqualsTrue());
            return doesNullEqualsTrue();
        }

        LocalDateTime dateAsLocalDateTime = getAsLocalDateTime(dateToValidate);

        final boolean match = getValues().stream()
                .anyMatch(value -> Equals.equalsUntyped(dateAsLocalDateTime.getYear(), value));
        log.debug("'{}' is{}" + " within one year of {}", dateToValidate, (match ? "" : " NOT"), getValues());
        return match;
    }

}
