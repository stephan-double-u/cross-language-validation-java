package de.swa.clv.constraints;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.time.LocalDate;

public class EqualsAny extends Equals {

    private static final Logger log = LoggerFactory.getLogger(EqualsAny.class);

    public static final String TOKEN = "EQUALS_ANY";

    EqualsAny(final boolean nullEqualsTrue, String... values) {
        setObjectValues(getValuesWithAllowFlagAsObjectList(nullEqualsTrue, values));
    }

    EqualsAny(final boolean nullEqualsTrue, final Enum<?>... values) {
        setObjectValues(getValuesWithAllowFlagAsObjectList(nullEqualsTrue, values));
    }

    EqualsAny(final boolean nullEqualsTrue, final Number... values) {
        setObjectValues(getValuesWithAllowFlagAsObjectList(nullEqualsTrue, values));
    }

    EqualsAny(final boolean nullEqualsTrue, final Boolean... values) {
        setObjectValues(getValuesWithAllowFlagAsObjectList(nullEqualsTrue, values));
    }

    EqualsAny(final boolean nullEqualsTrue, final LocalDate... values) {
        setObjectValues(getValuesWithAllowFlagAsObjectList(nullEqualsTrue, values));
    }

    @Override
    public String getType() {
        return TOKEN;
    }

    @Override
    public boolean validate(final Object objectToValidate, final Object notRelevant) {
        final Boolean nullEqualsTrue = (Boolean) getValues().get(0);
        if (objectToValidate == null) {
            log.debug("'Null object equals to {}", nullEqualsTrue);
            return nullEqualsTrue;
        }
        final boolean match = getValues().stream()
                .skip(1)
                .anyMatch(value -> Equals.equals(objectToValidate, value));
        log.debug("'{}' does{}" + " equals one of {}", objectToValidate, (match ? "" : " NOT"), getValues());
        return match;
    }

}
