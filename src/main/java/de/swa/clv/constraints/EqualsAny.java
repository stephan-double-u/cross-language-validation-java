package de.swa.clv.constraints;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.time.LocalDate;
import java.util.Arrays;
import java.util.List;

public class EqualsAny extends EqualsRoot {

    private static final Logger log = LoggerFactory.getLogger(EqualsAny.class);

    EqualsAny(final String... values) {
        setObjectValues(Arrays.asList(values));
    }

    EqualsAny(final Enum<?>... values) {
        setObjectValues(Arrays.asList(values));
    }

    EqualsAny(final Number... values) {
        setObjectValues(Arrays.asList(values));
    }

    EqualsAny(final Boolean... value) {
        setObjectValues(Arrays.asList(value));
    }

    EqualsAny(final LocalDate... value) {
        setObjectValues(Arrays.asList(value));
    }

    @Override
    public String getType() {
        return "EQUALS_ANY";
    }

    @Override
    public boolean validate(final Object objectToValidate, final Object notRelevant) {
        final boolean match = getValues().stream().anyMatch(value -> EqualsRoot.equals(objectToValidate, value));
        log.debug("'{}' does{}" + " equals one of {}", objectToValidate, (match ? "" : " NOT"), getValues());
        return match;
    }

}
