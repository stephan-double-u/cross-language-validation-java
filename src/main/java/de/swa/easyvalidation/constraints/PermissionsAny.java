package de.swa.easyvalidation.constraints;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Arrays;
import java.util.Objects;

public class PermissionsAny extends Permissions {

    private static Logger log = LoggerFactory.getLogger(Dates.class);

    PermissionsAny(final String... values) {
        setStringValues(Arrays.asList(values));
    }

    PermissionsAny(final Enum<?>... values) {
        setEnumValues(Arrays.asList(values));
    }

    @Override
    String getType() {
        return "PERMISSIONS_ANY";
    }

    @Override
    public boolean validate(final Object object, final Object contraintObjectIgnored) {
        final boolean match = getValues().stream().anyMatch(value -> Objects.equals(value, object));
        log.debug("'" + object + "' does" + (match ? "" : " NOT") + " equals any of " + getValues());
        return match;
    }


}
