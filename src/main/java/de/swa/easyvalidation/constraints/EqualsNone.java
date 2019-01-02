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
    
    EqualsNone(Enum<?>... values) {
        setEnumValues(Arrays.asList(values));
    }
    
    EqualsNone(Number... values) {
        setNumberValues(Arrays.asList(values));
    }

    EqualsNone(Boolean value) {
        setObjectValues(Arrays.asList(value));
    }

    EqualsNone(Object... values) {
        setObjectValues(Arrays.asList(values));
    }

    @Override
    String getType() {
        return "EQUALS_NONE";
    }

    @Override
    public boolean validate(Object object, Object contraintObjectIgnored) {
        final boolean match = getValues().stream().anyMatch(value -> Objects.equals(value, object));
        log.debug("'" + object + "' does" + (match ? " NOT" : "") + " equals none of " + getValues());
        return !match;
    }

}
