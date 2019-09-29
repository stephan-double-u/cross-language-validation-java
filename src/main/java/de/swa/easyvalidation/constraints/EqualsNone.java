package de.swa.easyvalidation.constraints;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.time.LocalDate;
import java.util.Arrays;

public class EqualsNone extends Equals {

    private static Logger log = LoggerFactory.getLogger(EqualsNone.class);

    EqualsNone(final String... values) {
        setObjectValues(Arrays.asList((Object[]) values));
    }
    
    EqualsNone(final Enum<?>... values) {
        setObjectValues(Arrays.asList((Object[]) values));
    }
    
    EqualsNone(final Number... values) {
        setObjectValues(Arrays.asList((Object[]) values));
    }

    EqualsNone(final Boolean... value) {
        setObjectValues(Arrays.asList((Object[]) value));
    }

    EqualsNone(final LocalDate... value) {
        setObjectValues(Arrays.asList((Object[]) value));
    }

    @Override
    String getType() {
        return "EQUALS_NONE";
    }

    @Override
    public boolean validate(final Object object, final Object constraintObjectIgnored) {
        if (object == null) {
            return true;
        }
        final boolean match = getValues().stream().anyMatch(value -> Equals.equals(value, object));
        log.debug("'" + object + "' does" + (match ? "" : " NOT") + " equals one of " + getValues() + " but should not");
        return !match;
    }

}
