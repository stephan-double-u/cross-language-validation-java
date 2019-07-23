package de.swa.easyvalidation.constraints;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.sql.JDBCType;
import java.util.Arrays;
import java.util.Objects;

public class EqualsNone extends Equals {

    private static Logger log = LoggerFactory.getLogger(EqualsNone.class);

    EqualsNone(String... values) {
        setStringValues(Arrays.asList(values));
    }
    
    EqualsNone(final Enum<?>... values) {
        setEnumValues(Arrays.asList(values));
    }
    
    EqualsNone(final Number... values) {
        setNumberValues(Arrays.asList(values));
    }

    EqualsNone(final Boolean value) {
        setObjectValues(Arrays.asList(value));
    }

    EqualsNone(final Object... values) {
        setObjectValues(Arrays.asList(values));
    }

    @Override
    String getType() {
        return "EQUALS_NONE";
    }

    @Override
    public boolean validate(final Object object, final Object contraintObjectIgnored) {
        final boolean match = getValues().stream().anyMatch(value -> Objects.equals(value, object));
        log.debug("'" + object + "' does" + (match ? "" : " NOT") + " equals one of " + getValues() + " but should not");
        return !match;
    }

}
