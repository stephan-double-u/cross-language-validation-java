package de.swa.clv.constraints;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Arrays;
import java.util.List;

public class PermissionsAny extends Permissions {

    private static final Logger log = LoggerFactory.getLogger(PermissionsAny.class);

    PermissionsAny(final String... values) {
        setValues(Arrays.asList((Object[]) values));
    }

    @Override
    public String getToken() {
        return "ANY";
    }

    @Override
    public boolean validate(final List<Object> userPermissions) {
        final boolean match = getValues().stream()
                .anyMatch(userPermissions::contains);
        log.debug("'" + userPermissions + "' does" + (match ? "" : " NOT") + " equals any of " + getValues());
        return match;
    }

    @Override
    public boolean validate(Object value, Object constraintObject) {
        throw new IllegalStateException("Should not be called");
    }

}
