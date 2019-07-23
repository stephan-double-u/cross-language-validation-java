package de.swa.easyvalidation.constraints;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Arrays;
import java.util.Objects;

public class EqualsAny extends Equals {

    private static Logger log = LoggerFactory.getLogger(EqualsAny.class);

    //TODO support CharSequence instead?
    EqualsAny(final String... values) {
        setStringValues(Arrays.asList(values));
    }

    EqualsAny(final Enum<?>... values) {
        setEnumValues(Arrays.asList(values));
    }

    EqualsAny(final Number... values) {
        setNumberValues(Arrays.asList(values));
    }

    EqualsAny(final Boolean value) {
        setObjectValues(Arrays.asList(value));
    }

    EqualsAny(final Object... values) {
        setObjectValues(Arrays.asList(values));
    }

    @Override
    String getType() {
        return "EQUALS_ANY";
    }

    @Override
    public boolean validate(final Object object, final Object contraintObjectIgnored) {
        final boolean match = getValues().stream().anyMatch(value -> Objects.equals(value, object));
        log.debug("'" + object + "' does" + (match ? "" : " NOT") + " equals one of " + getValues());
        return match;
    }
}
