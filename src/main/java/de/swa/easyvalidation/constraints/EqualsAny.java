package de.swa.easyvalidation.constraints;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Arrays;
import java.util.Objects;

public class EqualsAny extends Equals {

    private static Logger log = LoggerFactory.getLogger(EqualsAny.class);

    //TODO support CharSequence instead?
    EqualsAny(String... values) {
        setStringValues(Arrays.asList(values));
    }

    EqualsAny(Enum<?>... values) {
        setEnumValues(Arrays.asList(values));
    }

    EqualsAny(Number... values) {
        setNumberValues(Arrays.asList(values));
    }

    EqualsAny(Boolean value) {
        setObjectValues(Arrays.asList(value));
    }

    EqualsAny(Object... values) {
        setObjectValues(Arrays.asList(values));
    }

    @Override
    String getType() {
        return "EQUALS_ANY";
    }

    @Override
    public boolean validate(Object object, Object contraintObjectIgnored) {
        final boolean match = getValues().stream().anyMatch(value -> Objects.equals(value, object));
        log.debug("'" + object + "' does" + (match ? "" : " NOT") + " equals any of " + getValues());
        return match;
    }
}
