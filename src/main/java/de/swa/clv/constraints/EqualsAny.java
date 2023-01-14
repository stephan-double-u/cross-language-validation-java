package de.swa.clv.constraints;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.time.LocalDate;

class EqualsAny extends EqualsConstraint implements IsCreateConstraint, IsUpdateConstraint {

    private static final Logger log = LoggerFactory.getLogger(EqualsAny.class);

    public static final String TOKEN = "EQUALS_ANY";

    EqualsAny(final boolean nullEqualsTrue, String... values) {
        setNullEqualsTrue(nullEqualsTrue);
        setValues(getValuesAsObjectList(values));
    }

    EqualsAny(final boolean nullEqualsTrue, final Enum<?>... values) {
        setNullEqualsTrue(nullEqualsTrue);
        setValues(getValuesAsObjectList(values));
    }

    EqualsAny(final boolean nullEqualsTrue, final Number... values) {
        setNullEqualsTrue(nullEqualsTrue);
        setValues(getValuesAsObjectList(values));
    }

    EqualsAny(final boolean nullEqualsTrue, final Boolean... values) {
        setNullEqualsTrue(nullEqualsTrue);
        setValues(getValuesAsObjectList(values));
    }

    EqualsAny(final boolean nullEqualsTrue, final LocalDate... values) {
        setNullEqualsTrue(nullEqualsTrue);
        setValues(getValuesAsObjectList(values));
    }

    @Override
    public String getToken() {
        return TOKEN;
    }

    @Override
    public boolean validate(final Object objectToValidate, final Object notRelevant) {
        if (objectToValidate == null) {
            log.debug("'Null object equals to {}", doesNullEqualsTrue());
            return doesNullEqualsTrue();
        }
        final boolean match = getValues().stream()
                .anyMatch(value -> equalsUntyped(objectToValidate, value));
        log.debug("'{}' does{}" + " equals one of {}", objectToValidate, (match ? "" : " NOT"), getValues());
        return match;
    }

}
