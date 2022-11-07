package de.swa.clv.constraints;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.time.LocalDateTime;
import java.time.temporal.IsoFields;

/**
 * Constraint to express expectations for date quarters.
 */
public class QuarterAny extends Quarter {

    private static final Logger log = LoggerFactory.getLogger(QuarterAny.class);

    public static final String TOKEN = "QUARTER_ANY";

    QuarterAny(final boolean nullEqualsTrue, final Integer... quarters) {
        setNullEqualsTrue(nullEqualsTrue);
        setValues(getValuesAsObjectList(quarters));
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
        Integer dateQuarter = dateAsLocalDateTime.get(IsoFields.QUARTER_OF_YEAR);

        final boolean match = getValues().stream()
                .anyMatch(value -> Equals.equalsUntyped(dateQuarter, value));
        log.debug("'{}' is{}" + " within one quarter of {}", dateToValidate, (match ? "" : " NOT"), getValues());
        return match;
    }

}
