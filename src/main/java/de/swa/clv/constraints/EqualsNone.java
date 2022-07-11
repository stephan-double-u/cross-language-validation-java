package de.swa.clv.constraints;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.time.LocalDate;
import java.util.Arrays;

public class EqualsNone extends EqualsRoot {

    private static final Logger log = LoggerFactory.getLogger(EqualsNone.class);

    EqualsNone(final String... values) {
        setObjectValues(Arrays.asList(values));
    }
    
    EqualsNone(final Enum<?>... values) {
        setObjectValues(Arrays.asList(values));
    }
    
    EqualsNone(final Number... values) {
        setObjectValues(Arrays.asList(values));
    }

    EqualsNone(final Boolean... value) {
        setObjectValues(Arrays.asList(value));
    }

    EqualsNone(final LocalDate... value) {
        setObjectValues(Arrays.asList(value));
    }

    @Override
    public String getType() {
        return "EQUALS_NONE";
    }

    @Override
    public boolean validate(final Object objectToValidate, final Object constraintObjectIgnored) {
        final boolean match = getValues().stream().anyMatch(value -> EqualsRoot.equals(objectToValidate, value));
        log.debug("'" + objectToValidate + "' does" + (match ? "" : " NOT") + " equals one of " + getValues() + " but should not");
        return !match;
    }

}
