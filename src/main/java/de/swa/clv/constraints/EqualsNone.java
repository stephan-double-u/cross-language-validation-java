package de.swa.clv.constraints;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.time.LocalDate;

public class EqualsNone extends Equals {

    private static final Logger log = LoggerFactory.getLogger(EqualsNone.class);

    public static final String TOKEN = "EQUALS_NONE";

    EqualsNone(final boolean nullEqualsTrue, final String... values) {
        setNullEqualsTrue(nullEqualsTrue);
        setValues(getValuesAsObjectList(values));
    }

    EqualsNone(final boolean nullEqualsTrue, final Enum<?>... values) {
        setNullEqualsTrue(nullEqualsTrue);
        setValues(getValuesAsObjectList(values));
    }
    
    EqualsNone(final boolean nullEqualsTrue, final Number... values) {
        setNullEqualsTrue(nullEqualsTrue);
        setValues(getValuesAsObjectList(values));
    }

    EqualsNone(final boolean nullEqualsTrue, final Boolean... values) {
        setNullEqualsTrue(nullEqualsTrue);
        setValues(getValuesAsObjectList(values));
    }

    EqualsNone(final boolean nullEqualsTrue, final LocalDate... values) {
        setNullEqualsTrue(nullEqualsTrue);
        setValues(getValuesAsObjectList(values));
    }

    @Override
    public String getToken() {
        return TOKEN;
    }

    @Override
    public boolean validate(final Object objectToValidate, final Object constraintObjectIgnored) {
        if (objectToValidate == null) {
            log.debug("'Null object equals to {}", doesNullEqualsTrue());
            return doesNullEqualsTrue();
        }
        final boolean match = getValues().stream()
                .anyMatch(value -> Equals.equalsUntyped(objectToValidate, value));
        log.debug("'{}' does{}" + " equals one of {}{}", objectToValidate, (match ? "" : " not"), getValues(),
                (match ? " (but should not)" : ""));
        return !match;
    }

}
