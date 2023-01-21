package de.swa.clv.constraints;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Arrays;
import java.util.List;

public class PermissionsAll extends Permissions {

    private static final Logger log = LoggerFactory.getLogger(PermissionsAll.class);

    PermissionsAll(final String... values) {
        setValues(Arrays.asList((Object[]) values));
    }

    @Override
    public String getToken() {
        return "ALL";
    }

    @Override
    public boolean validate(List<Object> userPermissions) {
        final boolean match = userPermissions.containsAll(getValues());
        log.debug("'" + userPermissions + "' does" + (match ? "" : " NOT") + " equals all of " + getValues());
        return match;
    }

    @Override
    public boolean validate(Object value, Object constraintObject) {
        throw new IllegalStateException("Should not be called");
    }

}
