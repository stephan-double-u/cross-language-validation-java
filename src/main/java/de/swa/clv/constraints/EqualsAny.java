package de.swa.clv.constraints;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.time.LocalDate;

class EqualsAny extends EqualsConstraint implements IsCreateConstraint, IsUpdateConstraint {

    private static final Logger log = LoggerFactory.getLogger(EqualsAny.class);

    public static final String TOKEN = "EQUALS_ANY";

    EqualsAny(boolean nullEqualsTrue, String... values) {
        setNullEqualsTrue(nullEqualsTrue);
        setValues(getValuesAsObjectList(values));
    }

    EqualsAny(boolean nullEqualsTrue, Enum<?>... values) {
        setNullEqualsTrue(nullEqualsTrue);
        setValues(getValuesAsObjectList(values));
    }

    EqualsAny(boolean nullEqualsTrue, Number... values) {
        setNullEqualsTrue(nullEqualsTrue);
        setValues(getValuesAsObjectList(values));
    }

    EqualsAny(boolean nullEqualsTrue, Boolean... values) {
        setNullEqualsTrue(nullEqualsTrue);
        setValues(getValuesAsObjectList(values));
    }

    EqualsAny(boolean nullEqualsTrue, LocalDate... values) {
        setNullEqualsTrue(nullEqualsTrue);
        setValues(getValuesAsObjectList(values));
    }

    @Override
    public String getToken() {
        return TOKEN;
    }

    @Override
    public boolean validate(Object objectToValidate, Object notRelevant) {
        if (objectToValidate == null) {
            log.debug("'Null object equals to {}", doesNullEqualsTrue());
            return doesNullEqualsTrue();
        }
        boolean match = getValues().stream()
                .anyMatch(value -> equalsUntyped(objectToValidate, value));
        log.debug("'{}' does{}" + " equals one of {}", objectToValidate, (match ? "" : " NOT"), getValues());
        return match;
    }

}
